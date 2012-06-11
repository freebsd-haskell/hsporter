module Main where

import Control.Applicative
import Control.Monad
import qualified Data.Map as DM
import Data.Map ((!))
import Data.Maybe
import qualified Data.Text as DT
import Data.Version
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

getConfiguration :: FilePath -> IO [FilePath]
getConfiguration path = do
  contents <- (normalize . DT.lines . DT.pack) <$> readFile path
  let m = DM.fromList $ mapMaybe formatLine contents
  return $ [m ! "dbdir", m ! "portsdir", m ! "updatesdir"]
  where
    formatLine line =
      case (DT.strip <$> DT.splitOn (DT.pack "=") line) of
        (key:val:_) -> Just (DT.unpack . DT.toLower $ key,DT.unpack val)
        _           -> Nothing

showUpdates :: [FilePath] -> IO String
showUpdates files@(_:_:platform:_) = do
  (hdm,cpm,vcm,ports) <- initialize files
  baselibs <- Distribution.FreeBSD.Update.getBaseLibs platform
  let updates = learnUpdates hdm cpm vcm baselibs ports
  return . unlines . mapMaybe updateLine $ updates

fetchCabalFile :: FilePath -> PackageName -> Version -> IO ()
fetchCabalFile dbDir (PackageName pn) v = do
  let ver = showVersion v
  let uri = getCabalURI (pn,ver)
  let fn  = pn ++ "-" ++ ver
  putStr $ "Fetching " ++ fn ++ "..."
  downloadFile uri >>= writeFile (dbDir </> cabal fn)
  putStrLn "done."

resetDirectory :: FilePath -> IO ()
resetDirectory dir = do
  removeDirectoryRecursive dir
  createDirectoryIfMissing True dir

downloadCabalFiles :: FilePath -> [(PackageName,Category,Version)] -> HDM -> IO ()
downloadCabalFiles dbdir ports hdm = do
  resetDirectory dbdir
  forM_ ports $ \(p,_,v) -> do
    forM_ (filter (>= v) $ hdm ! p) $ \v ->
      fetchCabalFile dbdir p v

cachePortVersions :: FilePath -> IO ()
cachePortVersions portsDir = do
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

cacheDB :: FilePath -> FilePath -> IO ()
cacheDB dbDir platform = do
  ports <- getPortVersions portVersionsFile
  baselibs <- Distribution.FreeBSD.Update.getBaseLibs platform
  hdm <- buildHackageDatabase hackageLog baselibs
  downloadCabalFiles dbDir ports hdm

cache :: [FilePath] -> IO ()
cache [dbDir,portsDir,platform] = do
  putStrLn "Colllecting:"
  putStr "Port information..."
  cachePortVersions portsDir
  putStrLn "done."
  putStr "HackageDB information..."
  cacheHackageDB
  putStrLn "done."
  putStr "Cabal package descriptions..."
  cacheDB dbDir platform
  putStrLn "done."

downloadUpdates :: [FilePath] -> BuildOpts -> IO ()
downloadUpdates [dbDir,portsDir,updatesDir,plConf] opts = do
  putStrLn "Update starts."
  removeDirectoryRecursive updatesDir
  createDirectoryIfMissing True updatesDir
  (hdm,cpm,vcm,ports) <- initialize [dbDir,portsDir,plConf]
  baselibs <- Distribution.FreeBSD.Update.getBaseLibs plConf
  forM_ (learnUpdates hdm cpm vcm baselibs ports) $
    \(p@(PackageName pn),Category c,v,v1,_,_) -> do
      let [v',v1'] = showVersion <$> [v,v1]
      when (v < v1) $ do
        putStr $ printf "Updating port for %s (%s) (%s -> %s)..." pn c v' v1'
        dump <- readFile $ dbDir </> cabal (pn ++ "-" ++ v1')
        (ppath,port) <- buildPort opts dump (Just c)
        createPortFiles (updatesDir </> ppath) port
        putStrLn "done."
  putStrLn "Update finished."

checkCfg :: ([FilePath] -> IO ()) -> IO ()
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
  [ return "platform.conf"
  , return "ghc.conf"
  , return "categories.conf"
  ]
#endif

cmdPrintUpdates :: IO ()
cmdPrintUpdates = checkCfg $ \(dbDir:portsDir:_) -> do
  ghcConf <- getGhcConf
  platConf <- getPlatformConf
  let platform = ghcConf ++ platConf
  showUpdates [dbDir,portsDir,platform] >>= putStrLn

cmdDownloadUpdates :: IO ()
cmdDownloadUpdates = checkCfg $ \(dbDir:portsDir:updatesDir:_) -> do
  ghcConf <- getGhcConf
  platConf <- getPlatformConf
  catsConf <- getCategoriesConf
  let opts = BuildOpts ghcConf catsConf
  let platform = ghcConf ++ platConf
  downloadUpdates [dbDir,portsDir,updatesDir,platform] opts

cmdUpdatePortVersions :: IO ()
cmdUpdatePortVersions = checkCfg $ \(_:portsDir:_) -> do
  cachePortVersions portsDir

cmdGetLatestHackageVersions :: IO ()
cmdGetLatestHackageVersions = checkCfg $ \(dbDir:_) -> do
  cacheHackageDB
  ports <- getPortVersions portVersionsFile
  platConf <- getPlatformConf
  baselibs <- Distribution.FreeBSD.Update.getBaseLibs platConf
  hdm <- buildHackageDatabase hackageLog baselibs
  resetDirectory dbDir
  forM_ ports $ \(p@(PackageName pn),_,v) -> do
    let available = filter (>= v) $ hdm ! p
    if (not . null $ available)
      then do
        let latest = maximum available
        fetchCabalFile dbDir p latest
      else do
        putStrLn $ "Cannot be get: " ++ pn ++ ", " ++ showVersion v
        putStrLn $ "hdm: " ++ intercalate ", " (map showVersion (hdm ! p))

cmdFetchCabal :: String -> String -> IO ()
cmdFetchCabal name version = checkCfg $ \(dbDir:_) -> do
  files <- filter (f name) <$> getDirectoryContents dbDir
  mapM_ removeFile $ map (dbDir </>) files
  fetchCabalFile dbDir (PackageName name) (toVersion version)
  where
    f r x
      | null l    = False
      | otherwise = (reverse $ tail l) == r
      where
        l = snd . break (== '-') . reverse $ x

cmdPrintCabalVersions :: String -> IO ()
cmdPrintCabalVersions name = do
  platConf <- getPlatformConf
  baselibs <- Distribution.FreeBSD.Update.getBaseLibs platConf
  hdm <- buildHackageDatabase hackageLog baselibs
  let versions = intercalate ", " $ showVersion <$> hdm ! (PackageName name)
  putStrLn versions

cmdIsVersionAllowed :: String -> String -> IO ()
cmdIsVersionAllowed name version = checkCfg $ \(dbDir:portsDir:_) -> do
  platConf <- getPlatformConf
  (hdm,cpm,vcm,_) <- initialize [dbDir,portsDir,platConf]
  baselibs <- Distribution.FreeBSD.Update.getBaseLibs platConf
  let (rs,dp)     = isVersionAllowed hdm cpm vcm baselibs pk
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

body :: [FilePath] -> IO ()
body [dbDir,portsDir,updatesDir] = do
  ghcConf <- getGhcConf
  catsConf <- getCategoriesConf
  platConf <- getPlatformConf
  let opts = BuildOpts ghcConf catsConf
  cache [dbDir,portsDir,platConf]
  putStrLn "== Port Status Overview =="
  let baselibs = ghcConf ++ platConf
  s <- showUpdates [dbDir,portsDir,baselibs]
  putStrLn s
  putStrLn "== Actual Updates =="
  downloadUpdates [dbDir,portsDir,updatesDir,baselibs] opts

cfg :: FilePath
cfg = "hsupdater.conf"

main :: IO ()
main = do
  haveCfg <- doesFileExist cfg
  if haveCfg
    then getConfiguration cfg >>= body
    else putStrLn $ printf "No \"%s\" found.  Aborting." cfg
