{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Distribution.FreeBSD.Common where

import Control.Monad.Reader
import qualified Data.Map as DM
import Data.Version
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version

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
  }

-- "HsPorter Monad" (stack)
newtype HPM a = HPM {
    unHPM :: ReaderT Cfg IO a
  }
  deriving (Functor,Monad,MonadIO,MonadReader Cfg)

hackageURI :: String
hackageURI = "http://hackage.haskell.org/packages/archive/"
