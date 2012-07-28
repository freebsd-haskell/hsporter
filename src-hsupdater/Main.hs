{-# LANGUAGE CPP #-}
module Main where

import Control.Applicative
import Control.Monad
import qualified Data.Map as DM
import Data.Map ((!))
import Data.Maybe
import qualified Data.Text as DT
import Data.Version
import Distribution.FreeBSD.Common
import Distribution.FreeBSD.Port hiding (normalize)
import Distribution.FreeBSD.Update
import Distribution.Package
import System.Directory
import System.FilePath.Posix
import Text.Printf
#ifdef STANDALONE
import Paths_hsporter
#endif
import Data.List

getConfiguration :: FilePath -> IO Cfg
getConfiguration path = do
  contents <- (normalize . DT.lines . DT.pack) <$> readFile path
  let m = DM.fromList $ mapMaybe formatLine contents
  ghcConf <- getGhcConf
  catsConf <- getCategoriesConf
  ghcLibs <- readFile ghcConf
  platLibs <- getPlatformConf >>= readFile
  let platform = Platform $ ghcLibs ++ platLibs
  let baselibs = Distribution.FreeBSD.Update.getBaseLibs platform
  return $
    Cfg (m ! "dbdir") (m ! "portsdir") (m ! "updatesdir")
        platform (BuildOpts ghcConf catsConf) baselibs
  where
    formatLine line =
      case (DT.strip <$> DT.splitOn (DT.pack "=") line) of
        (key:val:_) -> Just (DT.unpack . DT.toLower $ key,DT.unpack val)
        _           -> Nothing

showUpdates :: Cfg -> IO String
showUpdates c = do
  (hdm,cpm,vcm,ports) <- initialize c
  let updates = learnUpdates hdm cpm vcm ports c
  return . unlines . mapMaybe updateLine $ updates

fetchCabalFile :: Cfg -> PackageName -> Version -> IO ()
fetchCabalFile c (PackageName pn) v = do
  let ver = showVersion v
  let uri = getCabalURI (pn,ver)
  let fn  = pn ++ "-" ++ ver
  putStr $ "Fetching " ++ fn ++ "..."
  downloadFile uri >>= writeFile ((cfgDbDir c) </> cabal fn)
  putStrLn "done."

resetDirectory :: FilePath -> IO ()
resetDirectory dir = do
  removeDirectoryRecursive dir
  createDirectoryIfMissing True dir

downloadCabalFiles :: Cfg -> [(PackageName,Category,Version)] -> HDM -> IO ()
downloadCabalFiles c ports hdm = do
  resetDirectory (cfgDbDir c)
  forM_ ports $ \(p,_,v) -> do
    forM_ (filter (>= v) $ hdm ! p) $ \v ->
      fetchCabalFile c p v

cachePortVersions :: Cfg -> IO ()
cachePortVersions c = do
  let portsDir = cfgPortsDir c
  let mk = portsDir </> bsdHackageMk
  contents <- (normalize . DT.lines . DT.pack) <$> readFile mk
  versions <- forM (map (DT.unpack . (!! 1) . DT.words) contents) $ \d -> do
    c <- readFile $ portsDir </> d </> mkfile
    let cat = takeDirectory d
    let k = filter (not . null) . map words . lines $ c
    let pnLine = head $ filter ((== "PORTNAME=") . head) k
    let pvLine = head $ filter ((== "PORTVERSION=") . head) k
    return $ unwords [pnLine !! 1, cat, pvLine !! 1]
  writeFile portVersionsFile $ unlines versions

cacheHackageDB :: IO ()
cacheHackageDB = downloadFile hackageLogURI >>= writeFile hackageLog

cacheDB :: Cfg -> IO ()
cacheDB c = do
  ports <- getPortVersions portVersionsFile
  let baselibs = Distribution.FreeBSD.Update.getBaseLibs (cfgPlatform c)
  hdm <- buildHackageDatabase hackageLog baselibs
  downloadCabalFiles c ports hdm

cache :: Cfg -> IO ()
cache c = do
  putStrLn "Colllecting:"
  putStr "Port information..."
  cachePortVersions c
  putStrLn "done."
  putStr "HackageDB information..."
  cacheHackageDB
  putStrLn "done."
  putStr "Cabal package descriptions..."
  cacheDB c
  putStrLn "done."

downloadUpdates :: Cfg -> IO ()
downloadUpdates c = do
  let updatesDir = cfgUpdatesDir c
  putStrLn "Update starts."
  removeDirectoryRecursive updatesDir
  createDirectoryIfMissing True updatesDir
  (hdm,cpm,vcm,ports) <- initialize c
  forM_ (learnUpdates hdm cpm vcm ports c) $
    \(p@(PackageName pn),Category ct,v,v1,_,_) -> do
      let [v',v1'] = showVersion <$> [v,v1]
      when (v < v1) $ do
        putStr $ printf "Updating port for %s (%s) (%s -> %s)..." pn ct v' v1'
        dump <- readFile $ (cfgDbDir c) </> cabal (pn ++ "-" ++ v1')
        (ppath,port) <- buildPort (cfgBuildOpts c) dump (Just ct)
        createPortFiles (updatesDir </> ppath) port
        putStrLn "done."
  putStrLn "Update finished."

checkCfg :: (Cfg -> IO ()) -> IO ()
checkCfg block = do
  haveCfg <- doesFileExist cfg
  if haveCfg
    then getConfiguration cfg >>= block
    else putStrLn $ printf "No \"%s\" found.  Aborting." cfg

[getPlatformConf,getGhcConf,getCategoriesConf] =
#ifdef STANDALONE
  [ getDataFileName "platform.conf"
  , getDataFileName "ghc.conf"
  , getDataFileName "categories.conf"
  ]
#else
  [ return "platform.conf"   :: IO FilePath
  , return "ghc.conf"        :: IO FilePath
  , return "categories.conf" :: IO FilePath
  ]
#endif

cmdPrintUpdates :: IO ()
cmdPrintUpdates = checkCfg $ \c -> do
  showUpdates c >>= putStrLn

cmdDownloadUpdates :: IO ()
cmdDownloadUpdates = checkCfg downloadUpdates

cmdUpdatePortVersions :: IO ()
cmdUpdatePortVersions = checkCfg cachePortVersions

cmdGetLatestHackageVersions :: IO ()
cmdGetLatestHackageVersions = checkCfg $ \c -> do
  cacheHackageDB
  ports <- getPortVersions portVersionsFile
  let baselibs = Distribution.FreeBSD.Update.getBaseLibs (cfgPlatform c)
  hdm <- buildHackageDatabase hackageLog baselibs
  resetDirectory (cfgDbDir c)
  forM_ ports $ \(p@(PackageName pn),_,v) -> do
    let available = filter (>= v) $ hdm ! p
    if (not . null $ available)
      then do
        let latest = maximum available
        fetchCabalFile c p latest
      else do
        putStrLn $ "Cannot be get: " ++ pn ++ ", " ++ showVersion v
        putStrLn $ "hdm: " ++ intercalate ", " (map showVersion (hdm ! p))

cmdFetchCabal :: String -> String -> IO ()
cmdFetchCabal name version = checkCfg $ \c -> do
  let dbDir = cfgDbDir c
  files <- filter (f name) <$> getDirectoryContents dbDir
  mapM_ removeFile $ map (dbDir </>) files
  fetchCabalFile c (PackageName name) (toVersion version)
  where
    f r x
      | null l    = False
      | otherwise = (reverse $ tail l) == r
      where
        l = snd . break (== '-') . reverse $ x

cmdPrintCabalVersions :: String -> IO ()
cmdPrintCabalVersions name = checkCfg $ \c -> do
  let baselibs = Distribution.FreeBSD.Update.getBaseLibs (cfgPlatform c)
  hdm <- buildHackageDatabase hackageLog baselibs
  let versions = intercalate ", " $ showVersion <$> hdm ! (PackageName name)
  putStrLn versions

cmdIsVersionAllowed :: String -> String -> IO ()
cmdIsVersionAllowed name version = checkCfg $ \c -> do
  (hdm,cpm,vcm,_) <- initialize c
  let (rs,dp)     = isVersionAllowed hdm cpm vcm c pk
  let restricted  = [ p | ((PackageName p,_),_) <- rs ]
  let unsatisfied = [ d | (PackageName d,_) <- dp ]
  when (not . null $ restricted) $
    putStrLn $ "Restricted by:  " ++ intercalate ", " restricted
  when (not . null $ unsatisfied) $
    putStrLn $ "Unsatisfied by: " ++ intercalate ", " unsatisfied
  when (null restricted && null unsatisfied) $
    putStrLn "OK!"
  where
    pk = (PackageName name, toVersion version)

body :: Cfg -> IO ()
body c = do
  cache c
  putStrLn "== Port Status Overview =="
  showUpdates c >>= putStrLn
  putStrLn "== Actual Updates =="
  downloadUpdates c

cfg :: FilePath
cfg = "hsupdater.conf"

main :: IO ()
main = do
  haveCfg <- doesFileExist cfg
  if haveCfg
    then checkCfg body
    else putStrLn $ printf "No \"%s\" found.  Aborting." cfg
