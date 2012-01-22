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
import Paths_hsporter

getConfiguration :: FilePath -> IO [FilePath]
getConfiguration path = do
  contents <- (normalize . DT.lines . DT.pack) <$> readFile path
  let m = DM.fromList $ mapMaybe formatLine contents
  return $ [m ! "dbdir", m ! "portsdir", m ! "updatesdir"]
  where
    formatLine line =
      case (DT.splitOn (DT.pack "=") line) of
        (key:val:_) -> Just (DT.unpack . DT.toLower $ key,DT.unpack val)
        _           -> Nothing

showUpdates :: [FilePath] -> IO String
showUpdates files = do
  (hdm,cpm,vcm,ports) <- initialize files
  let updates = learnUpdates hdm cpm vcm ports
  return $ unlines $ updateLine <$> updates

downloadCabalFiles :: FilePath -> [(PackageName,Category,Version)] -> HDM -> IO ()
downloadCabalFiles dbdir ports hdm = do
  removeDirectoryRecursive dbdir
  createDirectoryIfMissing True dbdir
  forM_ ports $ \(p@(PackageName pn),_,v) -> do
    forM_ (filter (>= v) $ hdm ! p) $ \v -> do
      let ver = showVersion v
      let uri = getCabalURI (pn,ver)
      let fn = pn ++ "-" ++ ver
      putStr $ "Fetching " ++ fn ++ "..."
      downloadFile uri >>= writeFile (dbdir </> cabal fn)
      putStrLn "done."

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

cacheDB :: FilePath -> IO ()
cacheDB dbDir = do
  ports <- getPortVersions portVersionsFile
  hdm <- buildHackageDatabase hackageLog
  downloadCabalFiles dbDir ports hdm

cache :: [FilePath] -> IO ()
cache [dbDir,portsDir] = do
  putStrLn "Colllecting:"
  putStr "Port information..."
  cachePortVersions portsDir
  putStrLn "done."
  putStr "HackageDB information..."
  cacheHackageDB
  putStrLn "done."
  putStr "Cabal package descriptions..."
  cacheDB dbDir
  putStrLn "done."

downloadUpdates :: [FilePath] -> BuildOpts -> IO ()
downloadUpdates [dbDir,portsDir,updatesDir,plConf] opts = do
  putStrLn "Update starts."
  removeDirectoryRecursive updatesDir
  createDirectoryIfMissing True updatesDir
  (hdm,cpm,vcm,ports) <- initialize [dbDir,portsDir,plConf]
  forM_ (learnUpdates hdm cpm vcm ports) $
    \(p@(PackageName pn),Category c,v1,v) -> do
      let [v1',v'] = showVersion <$> [v1,v]
      putStr $ printf "Updating port for %s (%s -> %s)..." pn v1' v'
      dump <- readFile $ dbDir </> cabal (pn ++ "-" ++ v')
      (ppath,port) <- buildPort opts dump (Just c)
      createPortFiles (updatesDir </> ppath) port
      putStrLn "done."
  putStrLn "Update finished."

body :: [FilePath] -> IO ()
body [dbDir,portsDir,updatesDir] = do
  coreConf <- getDataFileName "core.conf"
  catsConf <- getDataFileName "categories.conf"
  platConf <- getDataFileName "platform.conf"
  let opts = BuildOpts coreConf catsConf
  cache [dbDir,portsDir]
  downloadUpdates [dbDir,portsDir,updatesDir,platConf] opts

cfg :: FilePath
cfg = "hsupdater.conf"

main :: IO ()
main = do
  haveCfg <- doesFileExist cfg
  if haveCfg
    then getConfiguration cfg >>= body
    else putStrLn $ printf "No \"%s\" found.  Aborting." cfg

