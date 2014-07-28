module Distribution.FreeBSD.Update where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Data.Version
import qualified Data.Map as DM
import qualified Data.Text as DT
import Distribution.FreeBSD.Common
import Distribution.FreeBSD.Port hiding (normalize, getBaseLibs)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Version
import Network.HTTP
import System.Directory
import System.FilePath.Posix
import Text.ParserCombinators.ReadP
import Text.Printf
import Debug.Trace

toVersion :: String -> Version
toVersion = fst . last . readP_to_S parseVersion

hackageLog,portVersionsFile,bsdHackageMk,mkfile :: FilePath
[hackageLog,portVersionsFile,bsdHackageMk,mkfile] =
  [ "hackage.log"
  , "portversions"
  , "lang/ghc/bsd.hackage.mk"
  , "Makefile"
  ]

hackageLogURI :: String
hackageLogURI = "http://haskell.inf.elte.hu/hackage.log"

cabal :: FilePath -> FilePath
cabal = (<.> "cabal")

getCabalURI :: (String,String) -> String
getCabalURI (n,v) = hackageURI </> (n ++ "-" ++ v) </> cabal n

downloadFile :: String -> IO String
downloadFile url = simpleHTTP (getRequest url) >>= getResponseBody

getHackageDescription :: (PackageName,Version) -> IO GenericPackageDescription
getHackageDescription (PackageName p,v) = do
  dump <- downloadFile $ getCabalURI (p,showVersion v)
  ParseOk _ gpkg <- return $ parsePackageDescription dump
  return gpkg


buildHackageDatabase :: FilePath -> HPM HDM
buildHackageDatabase log = do
  core <- asks cfgBaseLibs
  entries <- liftIO $ map extractEntry . DT.lines . DT.pack <$> readFile log
  let retainCoreVersion pk@(pn,_) =
        case (lookup pn core) of
          Just v  -> (pn,[v])
          Nothing -> pk
  return $
    DM.fromList $ map retainCoreVersion $ map (packName &&& versions) $
    prepare entries
  where
    prepare = groupBy ((==) `on` fst) . sort

    packName = PackageName . DT.unpack . fst . head
    versions = map (toVersion . DT.unpack . snd)

    extractEntry line = (n,v)
      where
        (n:v:_) = DT.splitOn (DT.pack "/") line

buildCabalDatabase :: HPM CPM
buildCabalDatabase = do
  dbdir <- asks cfgDbDir
  contents <- liftIO $ getDirectoryContents dbdir
  let files = map (dbdir </>) $ filterDots contents
  entries <- mapM getEntry files
  return $ DM.fromList entries
  where
    filterDots = filter (flip notElem [".",".."])

    getEntry location = do
      let pack = package . packageDescription
      let file = takeFileName location
      dump <- liftIO $ readFile location
      result <- return $ parsePackageDescription dump
      case result of
        ParseOk _ gpkg ->
          return $ ((pkgName . pack &&& pkgVersion . pack) &&& id $ gpkg)
        _ -> fail $ printf "Could not parse description for \"%s\"." file

getCabalVersions :: CPM -> HDM
getCabalVersions =
  DM.fromList . map (\l -> (fst . head $ l, map snd l)) .
  groupBy ((==) `on` fst) . map fst .
  DM.toList

getDependencies :: GenericPackageDescription -> [Dependency]
getDependencies gpkgd = libdeps ++ exedeps
  where
    libdeps =
      case (condLibrary gpkgd) of
        Just x  -> condTreeConstraints x
        Nothing -> []

    exedeps = concatMap (condTreeConstraints . snd) . condExecutables $ gpkgd

normalize :: [DT.Text] -> [DT.Text]
normalize = filter nonEmpty . map (DT.strip . uncomment)
  where
    nonEmpty  = (not . DT.null)
    uncomment = DT.takeWhile (/= '#')

getBaseLibs :: Platform -> [(PackageName,Version)]
getBaseLibs (Platform p) = map translate . normalize . DT.lines . DT.pack $ p
  where
    translate l = (PackageName $ n,toVersion v)
      where (n:v:_) = DT.unpack <$> DT.words l

addConstraint :: (PackageName,Version,PackageName,VersionRange) -> VCM -> VCM
addConstraint (orig,v,p,vr) = DM.alter f p
  where
    f Nothing   = Just $ DM.singleton (orig,v) vr
    f (Just vs) = Just $ DM.insert (orig,v) vr vs

getVersionConstraints :: CPM -> VCM
getVersionConstraints = DM.fold f DM.empty
  where
    f :: GenericPackageDescription -> VCM -> VCM
    f p m = foldr addConstraint m $ translate <$> getDependencies p
      where
        translate (Dependency pk vr) = (packageName p,packageVersion p, pk,vr)

addCoreVersionConstraints :: VCM -> [(PackageName,Version)] -> VCM
addCoreVersionConstraints m = foldr addConstraint m . map translate
  where
    translate (p,v) = (p,v,p,thisVersion v)

sumVersionConstraints :: CPM -> [(PackageName,Version)] -> VCM
sumVersionConstraints cpm core =
  flip addCoreVersionConstraints core $ getVersionConstraints cpm

buildVersionConstraints :: CPM -> HPM VCM
buildVersionConstraints cpm = do
  baselibs <- fmap getBaseLibs $ asks cfgPlatform
  return $ sumVersionConstraints cpm baselibs

formatPackage :: (String,[Int]) -> (PackageName,Version)
formatPackage = first PackageName . second (flip Version [])

isVersionAllowed :: HDM -> CPM -> VCM
  -> (PackageName,Version)
  -> HPM ([((PackageName,Version),VersionRange)],[(PackageName,VersionRange)])
isVersionAllowed hdm cpm vcm i@(p,v) = do
  unsatisifed <- unsatisfiedDependencies hdm cpm vcm i
  return (constraints,unsatisifed)
  where
    constraints =
      case (DM.lookup p vcm) of
        Nothing  -> []
        Just res -> filter (versionFilter hdm cpm vcm i) $ DM.toList res

versionFilter :: HDM -> CPM -> VCM -> (PackageName,Version)
  -> ((PackageName,Version),VersionRange) -> Bool
versionFilter hdm cpm vcm (_,v) ((p,pv),vr)
  | not (v `withinRange` vr) = True
  | otherwise                = False

unsatisfiedDependencies :: HDM -> CPM -> VCM
  -> (PackageName,Version) -> HPM [(PackageName,VersionRange)]
unsatisfiedDependencies hdm cpm vcm (p,v) = do
  baselibs <- asks cfgBaseLibs
  let f (Dependency pk vr) =
        case (lookup pk baselibs) of
          Just v  -> not $ v `withinRange` vr
          Nothing -> not $ all ((`withinRange` vr)) $ hdm %!% pk
  return
    [ (pn,vr)
    | Dependency pn vr <- filter f $ getDependencies $ cpm %!% (p,v) ]

satisfyingDependencies :: HDM -> CPM
  -> (PackageName,Version) -> HPM [(PackageName,Version)]
satisfyingDependencies hdm cpm (p,v) = do
  baselibs <- asks cfgBaseLibs
  let f (Dependency pk vr) =
        case (lookup pk baselibs) of
          Nothing -> Just $ (pk, maximum allowed)
          _ -> Nothing
          where allowed = filterAllowed vr pk
  return $ mapMaybe f $ getDependencies $ cpm %!% (p,v)
  where
    filterAllowed vr pk = filter (`withinRange` vr) $ hdm %!% pk

getPortVersions :: FilePath -> HPM Ports
getPortVersions fn = do
  contents <- (map DT.words . DT.lines . DT.pack) <$> liftIO (readFile fn)
  return $ Ports $ sort $ (translate . map DT.unpack) <$> contents
  where
    translate (n:c:v:_) = (PackageName n,Category c,toVersion v)

isThereUpdate :: HDM -> CPM -> VCM -> (PackageName,Category,Version)
  -> HPM (Maybe PortUpdate)
isThereUpdate hdm cpm vcm (p,ct,v) = do
  let versions     = hdm %!% p
  let candidates   = (repeat p) `zip` versions
  checked <- mapM (isVersionAllowed hdm cpm vcm) candidates
  let allowed      = versions `zip` checked
  let f (_,(x,y))  = length x + length y
  let (v',(r',d')) = minimumBy (compare `on` f) allowed
  return $
    if (v /= v')
      then Just $ PU
        { puPackage       = p
        , puCategory      = ct
        , puOldVersion    = v
        , puNewVersion    = v'
        , puRestrictedBy  = map (fst . fst) r'
        , puUnsatisfiedBy = map fst d'
        }
      else Nothing

learnUpdates :: (HDM,CPM,VCM,Ports) -> HPM [PortUpdate]
learnUpdates (hdm,cpm,vcm,Ports ports) =
  fmap catMaybes $ mapM (isThereUpdate hdm cpm vcm) ports

prettyUpdateLine :: PortUpdate -> Maybe String
prettyUpdateLine
  (PU { puPackage       = PackageName p
      , puCategory      = Category c
      , puOldVersion    = v
      , puNewVersion    = v1
      , puRestrictedBy  = rs
      , puUnsatisfiedBy = dp
      })
  | v < v1 && null rs && null dp = Just $
    printf "%-32s %-12s ---> %-12s" port v' v1'
  | v < v1 && null rs = Just $
    printf "%-32s %-12s -/-> %-12s (U: %s)" port v' v1' udeps
  | v < v1 && null dp = Just $
    printf "%-32s %-12s -/-> %-12s (R: %s)" port v' v1' restricts
  | v < v1 = Just $
    printf "%-32s %-12s -/-> %-12s (R: %s, U: %s)" port v' v1' restricts udeps
  | otherwise = Nothing
  where
    port      = (printf "%s (%s)" p c) :: String
    [v',v1']  = showVersion <$> [v,v1]
    restricts = intercalate ", " [ p | PackageName p <- rs ]
    udeps     = intercalate ", " [ d | PackageName d <- dp ]

compactUpdateLine :: PortUpdate -> Maybe String
compactUpdateLine
  (PU { puPackage    = PackageName p
      , puCategory   = Category c
      , puOldVersion = v
      , puNewVersion = v1
      }) =
  Just $ printf "%s/hs-%s: %s --> %s" c p (showVersion v) (showVersion v1)

initialize :: HPM (HDM,CPM,VCM,Ports)
initialize = do
  cpm    <- buildCabalDatabase
  let hdm = getCabalVersions cpm
  vcm    <- buildVersionConstraints cpm
  ports  <- getPortVersions portVersionsFile
  return $ (hdm,cpm,vcm,ports)

createPortFiles :: FilePath -> Port -> IO ()
createPortFiles path port = do
  createDirectoryIfMissing True path
  writeFile (path </> "Makefile") $ makefile port
  writeFile (path </> "distinfo") $ distinfo port
  writeFile (path </> "pkg-descr") $ pkgDescr port
