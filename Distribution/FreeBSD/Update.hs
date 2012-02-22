module Distribution.FreeBSD.Update where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.Version
import Data.Map ((!))
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

-- FreeBSD Port Category
newtype Category = Category String
  deriving (Show,Eq,Ord)

-- "Cabal Package Map"
type CPM = DM.Map (PackageName,Version) GenericPackageDescription
-- "Version Constraint Map"
type VCM = DM.Map PackageName VersionRange
-- "HackageDB Map"
type HDM = DM.Map PackageName [Version]

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

buildHackageDatabase :: FilePath -> IO HDM
buildHackageDatabase log = do
  entries <- map extractEntry . DT.lines . DT.pack <$> readFile log
  return $ DM.fromList $ map (packName &&& versions) $ prepare entries
  where
    prepare = groupBy ((==) `on` fst) . sort

    packName = PackageName . DT.unpack . fst . head
    versions = map (toVersion . DT.unpack . snd)

    extractEntry line = (n,v)
      where
        (n:v:_) = reverseTake 2 . DT.words $ line
        reverseTake n l = drop ((length l) - n) l

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

getBaseLibs :: FilePath -> IO [(PackageName,Version)]
getBaseLibs fn = do
  contents <- (DT.lines . DT.pack) <$> readFile fn
  return $ translate <$> normalize contents
  where
    translate l = (PackageName $ n,toVersion v)
      where (n:v:_) = DT.unpack <$> DT.words l

addConstraint :: (PackageName,VersionRange) -> VCM -> VCM
addConstraint (p,vr) = DM.alter f p
  where
    f Nothing   = Just vr
    f (Just v)  = Just $ vr `intersectVersionRanges` v

getVersionConstraints :: CPM -> VCM
getVersionConstraints = DM.fold f DM.empty
  where
    f :: GenericPackageDescription -> VCM -> VCM
    f p m = foldr addConstraint m $ translate <$> getDependencies p

    translate (Dependency v r) = (v,r)

addCoreVersionConstraints :: VCM -> [(PackageName,Version)] -> VCM
addCoreVersionConstraints m = foldr addConstraint m . map translate
  where
    translate (p,v) = (p,thisVersion v)

sumVersionConstraints :: CPM -> [(PackageName,Version)] -> VCM
sumVersionConstraints cpm core =
  flip addCoreVersionConstraints core $ getVersionConstraints cpm

buildVersionConstraints :: CPM -> FilePath -> IO VCM
buildVersionConstraints cpm core = do
  baselibs <- getBaseLibs core
  return $ sumVersionConstraints cpm baselibs

formatPackage :: (String,[Int]) -> (PackageName,Version)
formatPackage = first PackageName . second (flip Version [])

checkDependency :: HDM -> CPM -> VCM -> Dependency -> Bool
checkDependency hdm cpm vcm (Dependency p vr) =
  case (DM.lookup p hdm) of
    Nothing   -> False
    Just x    ->
      case (isVersionAllowed hdm cpm vcm <$> (repeat p `zip` vs)) of
        [] -> False
        b  -> or b
      where
        vs = filter (flip withinRange vr) x

isVersionAllowed :: HDM -> CPM -> VCM -> (PackageName,Version) -> Bool
isVersionAllowed hdm cpm vcm (p,v) =
  case (DM.lookup p vcm) of
    Nothing   -> True
    Just rng  -> (withinRange v rng) && dependenciesAllowed
      where
        deps =
          case (DM.lookup (p,v) cpm) of
            Nothing   -> Nothing
            Just x    -> Just . getDependencies $ x

        dependenciesAllowed =
          case deps of
            Nothing -> False
            Just dp -> and (checkDependency hdm cpm vcm <$> dp)

getPortVersions :: FilePath -> IO [(PackageName,Category,Version)]
getPortVersions fn = do
  contents <- (map DT.words . DT.lines . DT.pack) <$> readFile fn
  return $ sort $ (translate . map DT.unpack) <$> contents
  where
    translate (n:c:v:_) = (PackageName n,Category c,toVersion v)

learnUpdates :: HDM -> CPM -> VCM -> [(PackageName,Category,Version)]
  -> [(PackageName,Category,Version,Version,Version)]
learnUpdates hdm cpm vcm ports = foldr isThereUpdate [] ports
  where
    isThereUpdate :: (PackageName,Category,Version)
      -> [(PackageName,Category,Version,Version,Version)]
      -> [(PackageName,Category,Version,Version,Version)]
    isThereUpdate (p,c,v) ls
      | (not . null $ available) && (maximum available > v) =
        (p,c,v,maximum available,suggested) : ls
      | otherwise = ls
      where
        suggested
          | (not . null $ allowed) = snd $ maximum allowed
          | otherwise              = v

        allowed     = filter (isVersionAllowed hdm cpm vcm) candidates
        candidates  = (repeat p) `zip` (filter (>= v) $ hdm ! p)
        available   = snd <$> candidates

updateLine :: (PackageName,Category,Version,Version,Version) -> Maybe String
updateLine (PackageName p,_,v,max,sugg)
  | v == max && v   == sugg = Nothing
  | v <  max && max == sugg = Just $
    printf "%-32s %-12s ---> %-12s" p v' sugg'
  | v <  max && v   == sugg = Just $
    printf "%-32s %-12s -/-> %-12s" p v' max'
  | otherwise = Just $
    printf "%-32s %-12s -~-> %-12s" p v' sugg'
  where
    v'    = showVersion v
    max'  = showVersion max
    sugg' = showVersion sugg

initialize :: [FilePath] -> IO (HDM,CPM,VCM,[(PackageName,Category,Version)])
initialize [dbDir,portsDir,platformConf] = do
  hdm <- buildHackageDatabase hackageLog
  cpm <- buildCabalDatabase dbDir
  vcm <- buildVersionConstraints cpm platformConf
  ports <- getPortVersions portVersionsFile
  return $ (hdm,cpm,vcm,ports)

createPortFiles :: FilePath -> Port -> IO ()
createPortFiles path port = do
  createDirectoryIfMissing True path
  writeFile (path </> "Makefile") $ makefile port
  writeFile (path </> "distinfo") $ distinfo port
  writeFile (path </> "pkg-descr") $ pkgDescr port
