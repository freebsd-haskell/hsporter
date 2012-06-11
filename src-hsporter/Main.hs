module Main where

import Control.Monad
import Distribution.FreeBSD.Port
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
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
  putStrLn $ "Fetching " ++ url ++ "..."
  dump <- getDescription url
  let category | (length args > 1) = Just $ args !! 1
               | otherwise         = Nothing
  (dir,port) <- buildPort opts dump category
  putStrLn $ "Building port in " ++ dir ++ "..."
  createDirectoryIfMissing True dir
  putStr "Conversion in progress... ["
  writeFile (dir </> "Makefile") (makefile port)
  putStr $ sep "Makefile"
  writeFile (dir </> "distinfo") (distinfo port)
  putStr $ sep "distinfo"
  writeFile (dir </> "pkg-descr") (pkgDescr port)
  putStr $ sep "pkg-descr"
  putStrLn $ " ]"
  putStrLn $ "Do not forget to do a 'portlint -C'"
  where sep str = " " ++ str
