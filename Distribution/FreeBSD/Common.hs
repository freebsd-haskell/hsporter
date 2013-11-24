{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Distribution.FreeBSD.Common where

import Control.Monad.Reader
import Control.Monad.Error
import qualified Data.Map as DM
import Data.Version
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version
import Text.Printf

-- FreeBSD Port Category
newtype Category = Category String
  deriving (Show,Eq,Ord)

-- "Cabal Package Map"
type CPM = DM.Map (PackageName,Version) GenericPackageDescription
-- "Version Constraint Map"
type VCM = DM.Map PackageName (DM.Map (PackageName,Version) VersionRange)
-- "HackageDB Map"
type HDM = DM.Map PackageName [Version]

newtype Platform = Platform String
newtype Ports    = Ports [(PackageName,Category,Version)]

data BuildOpts = BuildOpts {
    baseLibConf     :: FilePath
  , categoriesConf  :: FilePath
  }

data Cfg = Cfg
  { cfgDbDir      :: String
  , cfgPortsDir   :: String
  , cfgUpdatesDir :: String
  , cfgPlatform   :: Platform
  , cfgBuildOpts  :: BuildOpts
  , cfgBaseLibs   :: [(PackageName,Version)]
  , cfgThreads    :: Int
  }

data PortUpdate = PU
  { puPackage       :: PackageName
  , puCategory      :: Category
  , puOldVersion    :: Version
  , puNewVersion    :: Version
  , puRestrictedBy  :: [PackageName]
  , puUnsatisfiedBy :: [PackageName]
  }

instance Show PortUpdate where
  show (PU { puPackage    = PackageName pn
           , puCategory   = Category ct
           , puOldVersion = v
           , puNewVersion = v1
           }) =
    printf "%s (%s) (%s -> %s)" pn ct v' v1'
    where
      [v',v1'] = fmap showVersion [v,v1]

-- "HsPorter Monad" (stack)
newtype HPM a = HPM {
    unHPM :: ErrorT String (ReaderT Cfg IO) a
  }
  deriving (Functor,Monad,MonadIO,MonadReader Cfg)

runHPM :: HPM a -> Cfg -> IO ()
runHPM h c = do
  result <- run h c
  case result of
    Left str  -> putStrLn $ "Error: " ++ str
    Right _   -> return ()
  where
    run = runReaderT . runErrorT . unHPM

hackageURI :: String
hackageURI = "http://hackage.haskell.org/package/"
