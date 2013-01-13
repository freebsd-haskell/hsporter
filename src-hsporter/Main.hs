module Main where

import Control.Monad
import Distribution.FreeBSD.Common
import Distribution.FreeBSD.Port
import Distribution.Package
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

main :: IO ()
main = do
  ghcConf <- getDataFileName "ghc.conf"
  catsConf <- getDataFileName "categories.conf"
  let opts = BuildOpts ghcConf catsConf
  args <- getArgs
  when (length args < 1) printUsage
  url <- return $ head args
  printf "Fetching %s...\n" url
  dump <- getDescription url
  let category | (length args > 1) = Just $ args !! 1
               | otherwise         = Nothing
  (dir,port) <- buildPort opts dump category
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
