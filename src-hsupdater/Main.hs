{-# LANGUAGE CPP #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Error
import Control.Concurrent
import Control.Concurrent.STM
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
import System.IO
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
        (read $ m ! "threads")
  where
    formatLine line =
      case (DT.strip <$> DT.splitOn (DT.pack "=") line) of
        (key:val:_) -> Just (DT.unpack . DT.toLower $ key,DT.unpack val)
        _           -> Nothing

showUpdates :: (PortUpdate -> Maybe String) -> HPM String
showUpdates format = do
  updates <- initialize >>= learnUpdates
  return . unlines . sort . mapMaybe format $ updates

fetchCabalFile :: PV -> HPM ()
fetchCabalFile pv = do
  liftIO $ putStr (show pv)
  fetchCabalFile' pv
  liftIO $ putStrLn "done."

fetchCabalFile' :: PV -> HPM ()
fetchCabalFile' pv@(PV (PackageName pn,v)) = do
  let uri = getCabalURI (pn,showVersion v)
  dbdir <- asks cfgDbDir
  liftIO $ downloadFile uri >>= writeFile (dbdir </> cabal (show pv))

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
      fetchCabalFile (PV (p,v))

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

fetchPort :: PortUpdate -> HPM ()
fetchPort
  (PU { puPackage    = p@(PackageName pn)
      , puCategory   = Category ct
      , puOldVersion = v
      , puNewVersion = v1
      }) =
  when (v < v1) $ do
    buildopts <- asks cfgBuildOpts
    dbdir <- asks cfgDbDir
    updatesDir <- asks cfgUpdatesDir
    liftIO $ do
      dump <- readFile $ dbdir </> cabal (show $ PV (p,v1))
      (ppath,port) <- buildPort buildopts dump (Just ct)
      createPortFiles (updatesDir </> ppath) port

downloadUpdates :: HPM ()
downloadUpdates = do
  updatesDir <- asks cfgUpdatesDir
  liftIO $ do
    putStrLn "Update starts."
    removeDirectoryRecursive updatesDir
    createDirectoryIfMissing True updatesDir
  initialize >>= learnUpdates >>= parallel "Downloaded: %s" fetchPort
  liftIO $ putStrLn "Update finished."

runCfg :: HPM () -> IO ()
runCfg block = do
  haveCfg <- doesFileExist cfg
  if haveCfg
    then getConfiguration cfg >>= runHPM block
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
cmdPrintUpdates = runCfg $
  showUpdates prettyUpdateLine >>= fmap liftIO putStrLn

cmdPrintUpdateLogs :: IO ()
cmdPrintUpdateLogs = runCfg $
  showUpdates compactUpdateLine >>= fmap liftIO putStrLn

cmdDownloadUpdates :: IO ()
cmdDownloadUpdates = runCfg downloadUpdates

cmdUpdatePortVersions :: IO ()
cmdUpdatePortVersions = runCfg cachePortVersions

data Task a = Do a | Done

newtype PV = PV (PackageName,Version)
  deriving Eq

instance Show PV where
  show (PV (PackageName n,v)) = printf "%s-%s" n (showVersion v)

displayDone :: Show a => String -> TChan a -> IO ()
displayDone fmt c =
  forever $ do
    d <- atomically (readTChan c)
    putStrLn (printf fmt (show d))
    hFlush stdout

worker :: (a -> HPM ()) -> TChan a -> TChan (Task a) -> HPM ()
worker cmd done queue = loop
  where
    loop = do
      job <- liftIO . atomically $ readTChan queue
      case job of
        Done  -> return ()
        Do d  -> do
          cmd d
          liftIO . atomically $ writeTChan done d
          loop

modifyTVar_ :: TVar a -> (a -> a) -> STM ()
modifyTVar_ tv f = readTVar tv >>= writeTVar tv . f

forkTimes :: Int -> Cfg -> TVar Int -> HPM () -> IO ()
forkTimes k cfg alive act =
  replicateM_ k . forkIO $ do
    runHPM act cfg
    (atomically $ modifyTVar_ alive (subtract 1))

parallel :: Show a => String -> (a -> HPM ()) -> [a] -> HPM ()
parallel fmt cmd queue = do
  cfg <- ask
  let k = cfgThreads cfg
  done    <- liftIO $ newTChanIO
  jobs    <- liftIO $ newTChanIO
  workers <- liftIO $ newTVarIO k
  liftIO $ do
    forkIO (displayDone fmt done)
    forkTimes k cfg workers (worker cmd done jobs)
    atomically $ mapM_ (writeTChan jobs . Do) queue
    atomically $ replicateM_ k (writeTChan jobs Done)
    atomically $ do
      running <- readTVar workers
      check (running == 0)

cmdGetLatestHackageVersions :: IO ()
cmdGetLatestHackageVersions = runCfg $ do
  liftIO $ putStrLn "Initializing..."
  liftIO $ cacheHackageDB
  Ports ports <- getPortVersions portVersionsFile
  hdm <- buildHackageDatabase hackageLog
  dbdir <- asks cfgDbDir
  liftIO $ resetDirectory dbdir
  queue <- fmap catMaybes $ forM ports $ \(p@(PackageName pn),_,v) -> do
    let available = hdm ! p
    if (not . null $ available)
      then
        return $ Just (PV (p, maximum available))
      else liftIO $ do
        putStrLn $ "Cannot be got: " ++ pn ++ ", " ++ showVersion v
        putStrLn $ "hdm: " ++ intercalate ", " (map showVersion (hdm ! p))
        return Nothing
  liftIO $ putStrLn "Fetching new versions..."
  parallel "Fetched: %s" fetchCabalFile' queue
  core <- asks cfgBaseLibs
  cpm <- buildCabalDatabase
  new <- fmap (nub . concat) $ forM (DM.toList cpm) $ \(_,gpkgd) -> do
    return $
      forM (getDependencies gpkgd) (\(Dependency pk _) -> do
        if (pk `elem` [name | (name,_) <- core])
          then return Nothing
          else do
            let available = hdm %!% pk
            let versions  = repeat pk `zip` available
            return $
              case (catMaybes $ flip DM.lookup cpm <$> versions) of
                [] -> Just (PV (pk, maximum available))
                _  -> Nothing)
      >>= catMaybes
  when (not . null $ new) $ do
    liftIO $ putStrLn "Fetching new dependencies..."
    parallel "Fetched: %s" fetchCabalFile' new

fetchCabal :: PV -> HPM ()
fetchCabal pv@(PV (p@(PackageName name),version)) = do
  dbDir <- asks cfgDbDir
  files <- liftIO $ filter (f name) <$> getDirectoryContents dbDir
  liftIO $ mapM_ removeFile $ map (dbDir </>) files
  fetchCabalFile' pv
  where
    f r x
      | null l    = False
      | otherwise = (reverse $ tail l) == r
      where
        l = snd . break (== '-') . reverse $ x

cmdFetchCabal :: String -> String -> IO ()
cmdFetchCabal n v = do
  putStr (printf "Fetching %s-%s..." n v)
  runCfg $ fetchCabal (PV (PackageName n,toVersion v))
  putStrLn "done."

cmdFetchLatestCabal :: String -> IO ()
cmdFetchLatestCabal n = runCfg $ do
  hdm <- buildHackageDatabase hackageLog
  let latest = last $ hdm ! (PackageName n)
  liftIO $ cmdFetchCabal n (showVersion latest)

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

allPruneableUpdates :: HPM [PV]
allPruneableUpdates = do
  updates <- initialize >>= learnUpdates
  return $ mapMaybe toPrune updates
  where
    toPrune (PU { puRestrictedBy = [], puUnsatisfiedBy = [] }) = Nothing
    toPrune (PU { puPackage = p, puOldVersion = v, puNewVersion = v1 })
      | v < v1 = Just $ PV (p,v)
    toPrune _  = Nothing

cmdShowPruneableBy :: HPM [PV] -> IO ()
cmdShowPruneableBy query = runCfg $ do
  ps <- map show <$> query
  liftIO . putStrLn $
    if (null ps)
      then "There are no pruneable updates."
      else unlines ps

cmdPruneBy :: HPM [PV] -> IO ()
cmdPruneBy query = do
  putStrLn "Initializing..."
  runCfg $ query >>= parallel "Pruned: %s" fetchCabal

body :: HPM ()
body = do
  cache
  updates <- showUpdates prettyUpdateLine
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
