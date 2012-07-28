module Distribution.FreeBSD.Update where

import Control.Applicative
import Control.Arrow
import Control.Monad
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

-- FreeBSD Port Category
newtype Category = Category String
  deriving (Show,Eq,Ord)

-- "Cabal Package Map"
type CPM = DM.Map (PackageName,Version) GenericPackageDescription
-- "Version Constraint Map"
type VCM = DM.Map PackageName (DM.Map (PackageName,Version) VersionRange)
-- "HackageDB Map"
type HDM = DM.Map PackageName [Version]

(%!%) :: (Show k, Ord k) => DM.Map k a -> k -> a
m %!% k =
  case (DM.lookup k m) of
    Just v  -> v
    Nothing -> error $ "No value for key \"" ++ show k ++ "\"."

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
hackageLogURI = hackageURI </> "log"

cabal :: FilePath -> FilePath
cabal = (<.> "cabal")

getCabalURI :: (String,String) -> String
getCabalURI (n,v) = hackageURI </> n </> v </> cabal n

downloadFile :: String -> IO String
downloadFile url = simpleHTTP (getRequest url) >>= getResponseBody

getHackageDescription :: (PackageName,Version) -> IO GenericPackageDescription
getHackageDescription (PackageName p,v) = do
  dump <- downloadFile $ getCabalURI (p,showVersion v)
  ParseOk _ gpkg <- return $ parsePackageDescription dump
  return gpkg

buildHackageDatabase :: FilePath -> [(PackageName,Version)] -> IO HDM
buildHackageDatabase log core = do
  entries <- map extractEntry . DT.lines . DT.pack <$> readFile log
  return $
    DM.fromList $ map retainCoreVersion $ map (packName &&& versions) $
    prepare entries
  where
    prepare = groupBy ((==) `on` fst) . sort

    packName = PackageName . DT.unpack . fst . head
    versions = map (toVersion . DT.unpack . snd)

    extractEntry line = (n,v)
      where
        (n:v:_) = reverseTake 2 . DT.words $ line
        reverseTake n l = drop ((length l) - n) l

    retainCoreVersion pk@(pn,_) =
      case (lookup pn core) of
        Just v  -> (pn,[v])
        Nothing -> pk

buildCabalDatabase :: FilePath -> IO CPM
buildCabalDatabase dbdir = do
  files <- map (dbdir </>) <$> filterDots <$> getDirectoryContents dbdir
  entries <- mapM getEntry files
  return $ DM.fromList entries
  where
    filterDots = filter (flip notElem [".",".."])

    getEntry location = do
      dump <- readFile location
      ParseOk _ gpkg <- return $ parsePackageDescription dump
      return $ ((pkgName . pack &&& pkgVersion . pack) &&& id $ gpkg)
      where pack = package . packageDescription

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

buildVersionConstraints :: CPM -> Platform -> VCM
buildVersionConstraints cpm = sumVersionConstraints cpm . getBaseLibs

formatPackage :: (String,[Int]) -> (PackageName,Version)
formatPackage = first PackageName . second (flip Version [])

isVersionAllowed :: HDM -> CPM -> VCM -> [(PackageName,Version)]
  -> (PackageName,Version)
  -> ([((PackageName,Version),VersionRange)],[(PackageName,VersionRange)])
isVersionAllowed hdm cpm vcm baselibs i@(p,v) = (constraints,unsatisifed)
  where
    constraints =
      case (DM.lookup p vcm) of
        Nothing  -> []
        Just res -> filter (versionFilter hdm cpm vcm i) $ DM.toList res

    unsatisifed = unsatisfiedDependencies hdm cpm vcm baselibs i

versionFilter :: HDM -> CPM -> VCM -> (PackageName,Version)
  -> ((PackageName,Version),VersionRange) -> Bool
versionFilter hdm cpm vcm (_,v) ((p,pv),vr)
  | not (v `withinRange` vr) = True
  | otherwise                = False

unsatisfiedDependencies :: HDM -> CPM -> VCM -> [(PackageName,Version)]
  -> (PackageName,Version) -> [(PackageName,VersionRange)]
unsatisfiedDependencies hdm cpm vcm baselibs (p,v) =
  [ (pn,vr) | Dependency pn vr <- filter f $ getDependencies $ cpm %!% (p,v) ]
  where
    f (Dependency pk vr) =
      case (lookup pk baselibs) of
        Just _  -> False
        Nothing -> not $ all ((`withinRange` vr)) $ hdm %!% pk

getPortVersions :: FilePath -> IO [(PackageName,Category,Version)]
getPortVersions fn = do
  contents <- (map DT.words . DT.lines . DT.pack) <$> readFile fn
  return $ sort $ (translate . map DT.unpack) <$> contents
  where
    translate (n:c:v:_) = (PackageName n,Category c,toVersion v)

isThereUpdate :: HDM -> CPM -> VCM -> [(PackageName,Version)]
  -> (PackageName,Category,Version)
  -> Maybe (PackageName,Category,Version,Version,[PackageName],[PackageName])
isThereUpdate hdm cpm vcm baselibs (p,c,v)
  | (v /= v') = Just (p,c,v,v', map (fst . fst) r', map fst d')
  | otherwise = Nothing
  where
    allowed      =
      versions `zip` (map (isVersionAllowed hdm cpm vcm baselibs) candidates)
    candidates   = (repeat p) `zip` versions
    versions     = filter (>= v) $ hdm %!% p
    (v',(r',d')) = minimumBy (compare `on` f) allowed
    f (_,(x,y))  = length x + length y

learnUpdates :: HDM -> CPM -> VCM -> [(PackageName,Version)]
  -> [(PackageName,Category,Version)]
  -> [(PackageName,Category,Version,Version,[PackageName],[PackageName])]
learnUpdates hdm cpm vcm baselibs ports =
  mapMaybe (isThereUpdate hdm cpm vcm baselibs) ports

updateLine :: (PackageName,Category,Version,Version,[PackageName],[PackageName])
  -> Maybe String
updateLine (PackageName p,Category c,v,v1,rs,dp)
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

initialize :: Cfg -> IO (HDM,CPM,VCM,[(PackageName,Category,Version)])
initialize c = do
  cpm <- buildCabalDatabase (cfgDbDir c)
  let hdm = getCabalVersions cpm
  let vcm = buildVersionConstraints cpm (cfgPlatform c)
  ports <- getPortVersions portVersionsFile
  return $ (hdm,cpm,vcm,ports)

createPortFiles :: FilePath -> Port -> IO ()
createPortFiles path port = do
  createDirectoryIfMissing True path
  writeFile (path </> "Makefile") $ makefile port
  writeFile (path </> "distinfo") $ distinfo port
  writeFile (path </> "pkg-descr") $ pkgDescr port
