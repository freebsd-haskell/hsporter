module Distribution.FreeBSD.Common where

newtype Platform = Platform String

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
  }

hackageURI :: String
hackageURI = "http://hackage.haskell.org/packages/archive/"
