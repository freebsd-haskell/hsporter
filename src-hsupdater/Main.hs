{-# LANGUAGE CPP #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
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

showUpdates :: HPM String
showUpdates = do
  (hdm,cpm,vcm,ports) <- initialize
  updates <- learnUpdates hdm cpm vcm ports
  return . unlines . mapMaybe updateLine $ updates

fetchCabalFile :: PackageName -> Version -> HPM ()
fetchCabalFile (PackageName pn) v = do
  let ver = showVersion v
  let uri = getCabalURI (pn,ver)
  let fn  = pn ++ "-" ++ ver
  liftIO $ putStr $ "Fetching " ++ fn ++ "..."
  dbdir <- asks cfgDbDir
  liftIO $ downloadFile uri >>= writeFile (dbdir </> cabal fn)
  liftIO $ putStrLn "done."

resetDirectory :: FilePath -> IO ()
resetDirectory dir = do
  removeDirectoryRecursive dir
  createDirectoryIfMissing True dir

downloadCabalFiles :: Ports -> HDM -> HPM ()
downloadCabalFiles (Ports ports) hdm = do
  dbdir <- asks cfgDbDir
  liftIO $ resetDirectory dbdir
  forM_ ports $ \(p,_,v) -> do
    forM_ (filter (>= v) $ hdm ! p) $ \v ->
      fetchCabalFile p v

cachePortVersions :: HPM ()
cachePortVersions = do
  portsDir <- asks cfgPortsDir
  let mk = portsDir </> bsdHackageMk
  contents <- liftIO $ (normalize . DT.lines . DT.pack) <$> readFile mk
  versions <- forM (map (DT.unpack . (!! 1) . DT.words) contents) $ \d -> do
    port <- liftIO $ readFile $ portsDir </> d </> mkfile
    let cat = takeDirectory d
    let k = filter (not . null) . map words . lines $ port
    let pnLine = head $ filter ((== "PORTNAME=") . head) k
    let pvLine = head $ filter ((== "PORTVERSION=") . head) k
    return $ unwords [pnLine !! 1, cat, pvLine !! 1]
  liftIO $ writeFile portVersionsFile $ unlines versions

cacheHackageDB :: IO ()
cacheHackageDB = downloadFile hackageLogURI >>= writeFile hackageLog

cacheDB :: HPM ()
cacheDB = do
  ports <- getPortVersions portVersionsFile
  hdm <- buildHackageDatabase hackageLog
  downloadCabalFiles ports hdm

cache :: HPM ()
cache = do
  liftIO $ do
    putStrLn "Colllecting:"
    putStr "Port information..."
  cachePortVersions
  liftIO $ do
    putStrLn "done."
    putStr "HackageDB information..."
    cacheHackageDB
    putStrLn "done."
    putStr "Cabal package descriptions..."
  cacheDB
  liftIO $ putStrLn "done."

downloadUpdates :: HPM ()
downloadUpdates = do
  updatesDir <- asks cfgUpdatesDir
  liftIO $ do
    putStrLn "Update starts."
    removeDirectoryRecursive updatesDir
    createDirectoryIfMissing True updatesDir
  (hdm,cpm,vcm,ports) <- initialize
  dbdir <- asks cfgDbDir
  updates <- learnUpdates hdm cpm vcm ports
  forM_ updates $
    \(p@(PackageName pn),Category ct,v,v1,_,_) -> do
      let [v',v1'] = showVersion <$> [v,v1]
      when (v < v1) $ do
        buildopts <- asks cfgBuildOpts
        liftIO $ do
          putStr $ printf "Updating port for %s (%s) (%s -> %s)..." pn ct v' v1'
          dump <- readFile $ dbdir </> cabal (pn ++ "-" ++ v1')
          (ppath,port) <- buildPort buildopts dump (Just ct)
          createPortFiles (updatesDir </> ppath) port
          putStrLn "done."
  liftIO $ putStrLn "Update finished."

runCfg :: HPM () -> IO ()
runCfg block = do
  haveCfg <- doesFileExist cfg
  if haveCfg
    then getConfiguration cfg >>= runReaderT (unHPM block)
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
cmdPrintUpdates = runCfg $ showUpdates >>= fmap liftIO putStrLn

cmdDownloadUpdates :: IO ()
cmdDownloadUpdates = runCfg downloadUpdates

cmdUpdatePortVersions :: IO ()
cmdUpdatePortVersions = runCfg cachePortVersions

cmdGetLatestHackageVersions :: IO ()
cmdGetLatestHackageVersions = runCfg $ do
  liftIO $ cacheHackageDB
  Ports ports <- getPortVersions portVersionsFile
  hdm <- buildHackageDatabase hackageLog
  dbdir <- asks cfgDbDir
  liftIO $ resetDirectory dbdir
  forM_ ports $ \(p@(PackageName pn),_,v) -> do
    let available = filter (>= v) $ hdm ! p
    if (not . null $ available)
      then do
        let latest = maximum available
        fetchCabalFile p latest
      else liftIO $ do
        putStrLn $ "Cannot be get: " ++ pn ++ ", " ++ showVersion v
        putStrLn $ "hdm: " ++ intercalate ", " (map showVersion (hdm ! p))

cmdFetchCabal :: String -> String -> IO ()
cmdFetchCabal name version = runCfg $ do
  dbDir <- asks cfgDbDir
  files <- liftIO $ filter (f name) <$> getDirectoryContents dbDir
  liftIO $ mapM_ removeFile $ map (dbDir </>) files
  fetchCabalFile (PackageName name) (toVersion version)
  where
    f r x
      | null l    = False
      | otherwise = (reverse $ tail l) == r
      where
        l = snd . break (== '-') . reverse $ x

cmdPrintCabalVersions :: String -> IO ()
cmdPrintCabalVersions name = runCfg $ do
  hdm <- buildHackageDatabase hackageLog
  let versions = intercalate ", " $ showVersion <$> hdm ! (PackageName name)
  liftIO $ putStrLn versions

cmdIsVersionAllowed :: String -> String -> IO ()
cmdIsVersionAllowed name version = runCfg $ do
  (hdm,cpm,vcm,_) <- initialize
  (rs,dp) <- isVersionAllowed hdm cpm vcm pk
  let restricted  = [ p | ((PackageName p,_),_) <- rs ]
  let unsatisfied = [ d | (PackageName d,_) <- dp ]
  liftIO $ do
    when (not . null $ restricted) $
      putStrLn $ "Restricted by:  " ++ intercalate ", " restricted
    when (not . null $ unsatisfied) $
      putStrLn $ "Unsatisfied by: " ++ intercalate ", " unsatisfied
    when (null restricted && null unsatisfied) $
      putStrLn "OK!"
  where
    pk = (PackageName name, toVersion version)

body :: HPM ()
body = do
  cache
  updates <- showUpdates
  liftIO $ do
    putStrLn "== Port Status Overview =="
    putStrLn updates
    putStrLn "== Actual Updates =="
  downloadUpdates

cfg :: FilePath
cfg = "hsupdater.conf"

main :: IO ()
main = do
  haveCfg <- doesFileExist cfg
  if haveCfg
    then runCfg body
    else putStrLn $ printf "No \"%s\" found.  Aborting." cfg
