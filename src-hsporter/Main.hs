module Main where

import Control.Monad
import Data.Maybe
import Distribution.FreeBSD.Common
import Distribution.FreeBSD.Port
import Distribution.Package
import Distribution.PackageDescription
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import Text.Printf
import Paths_hsporter

printUsage :: IO ()
printUsage = do
  putStrLn "Usage: hsporter <URL to the .cabal> [port category]"
  exitSuccess

die :: String -> IO ()
die str = do
  putStrLn $ "Error: " ++ str
  exitFailure

main :: IO ()
main = do
  ghcConf <- getDataFileName "ghc.conf"
  catsConf <- getDataFileName "categories.conf"
  let opts = BuildOpts ghcConf catsConf
  args <- getArgs
  when (length args < 1) printUsage
  let (url:_) = args
  printf "Fetching %s...\n" url
  desc <- getDescriptionFromURL url
  case desc of
    Nothing -> die "Could not parse package description."
    Just p  -> do
      when (packageOf (packageDescription p) == "-") $
        die "Invalid package identifier."
  let gpkgd = fromJust desc
  let category | (length args > 1) = Just $ args !! 1
               | otherwise         = Nothing
  (dir,port) <- buildPort opts gpkgd category
  printf "Building port in %s...\n" dir
  createDirectoryIfMissing True dir
  putStr "Conversion in progress... ["
  writeFile (dir </> "Makefile") (makefile port)
  putStr $ sep "Makefile"
  writeFile (dir </> "distinfo") (distinfo port)
  putStr $ sep "distinfo"
  writeFile (dir </> "pkg-descr") (pkgDescr port)
  putStr $ sep "pkg-descr"
  putStrLn $ " ]"
  let PackageName n = name port
  putStrLn $ unlines
    [ "Do not forget to do a 'portlint -C'"
    , ""
    , "The corresponding bsd.hackage.mk line is as follows:"
    , printf "%-32s %s" (printf "%s_port=" n :: String) dir
    ]
  where sep str = " " ++ str
