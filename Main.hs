------------------------------------------------------------------------------
-- |
-- Module      : hsporter: convert cabal packages to FreeBSD ports
-- Copyright   : (c) Gabor Pali, 2010-2012
-- License     : BSD3
--
-- Maintainer  : FreeBSD Haskell <haskell@FreeBSD.org>
-- Stability   : experimental
-- Portability : non-portable
--
-- Create FreeBSD ports from Cabal packages according to the format and
-- conventions described in the FreeBSD Porter's Handbook \[1\].  It is used
-- in conjunction with special BSD make(1) include files which can be found in
-- the FreeBSD ports tree.
--
-- \[1\] http:\/\/www.freebsd.org\/doc\/en\/books\/porters-handbook\/
--
------------------------------------------------------------------------------

module Main where

import Codec.Archive.Tar as Tar
import Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS
import Data.Digest.Pure.SHA
import Data.List
import Data.Maybe
import Data.Version
import Distribution.ModuleName hiding (main)
import Distribution.Package
import Distribution.PackageDescription hiding (maintainer, category, license)
import Distribution.PackageDescription.Parse
import Distribution.Version
import Network.HTTP
import Network.URI
import System.Directory hiding (executable)
import System.Environment
import System.IO
import System.Time
import System.Exit (exitSuccess)
import Control.Monad (when)

hackageUrl, makefile, distinfo, pkgDescr :: String
[hackageUrl,makefile,distinfo,pkgDescr] =
  [ "http://hackage.haskell.org/packages/archive/"
  , "Makefile"
  , "distinfo"
  , "pkg-descr"
  ]

fullNameOf :: PackageDescription -> String
fullNameOf pkgd = prefix ++ nameOf pkgd

getDescription :: String -> IO String
getDescription url = do
  rsp <- simpleHTTP $ getRequest url
  getResponseBody rsp

getTarball :: String -> IO BS.ByteString
getTarball url = do
  uri <- return $ fromJust $ parseURI url
  rsp <- simpleHTTP $ request uri
  getResponseBody rsp
    where
      request uri = Request
        { rqURI = uri
        , rqMethod = GET
        , rqHeaders = []
        , rqBody = BS.empty }

tarballOf :: PackageDescription -> Bool -> String
tarballOf pkgd withUrl
  = url ++ nameOf pkgd ++ "-" ++ versionOf pkgd ++ ".tar.gz"
  where
    url =
      if withUrl
        then hackageUrl ++ nameOf pkgd ++ "/" ++ versionOf pkgd ++ "/"
        else ""

tarballIndex :: BS.ByteString -> [String]
tarballIndex tgz = foldEntries translate [] (\_ -> []) entries
  where
    translate entry ls = (entryPath entry):ls
    entries = (Tar.read . GZip.decompress) tgz

www :: String -> String
www url = "WWW:\t" ++ url

maintainer :: String -> String
maintainer m = "MAINTAINER=\t" ++ m

comment :: String -> String
comment c = "COMMENT=\t" ++ c

portname :: String -> String
portname n = "PORTNAME=\t" ++ n

portversion :: String -> String
portversion v = "PORTVERSION=\t" ++ v

categories :: String -> String
categories c = "CATEGORIES=\t" ++ cats
  where cats = foldl1 (++) $ intersperse " " $ [c] ++ ["haskell"]

prefix :: String
prefix = "hs-"

-- GHC 7.0.2
baseLibs :: [(String, [Int])]
baseLibs =
  [ ("Cabal", [1,10,1,0])
  , ("array", [0,3,0,2])
  , ("base", [4,3,1,0])
  , ("bytestring", [0,9,1,10])
  , ("containers", [0,4,0,0])
  , ("directory", [1,1,0,0])
  , ("extensible-exceptions", [0,1,1,2])
  , ("filepath", [1,2,0,0])
  , ("ghc", [7,0,2])
  , ("ghc-binary", [0,5,0,2])
  , ("ghc-prim", [0,2,0,0])
  , ("haskell2010", [1,0,0,0])
  , ("haskell98", [1,1,0,0])
  , ("hpc", [0,5,0,6])
  , ("integer-gmp", [0,2,0,3])
  , ("old-locale", [1,0,0,2])
  , ("old-time", [1,0,0,6])
  , ("pretty", [1,0,1,2])
  , ("process", [1,0,1,5])
  , ("random", [1,0,0,3])
  , ("template-haskell", [2,5,0,0])
  , ("time", [1,2,0,3])
  , ("unix", [2,4,2,0])
  ]

dependencies :: GenericPackageDescription -> [(String,String)]
dependencies gpkgd
  = sort $ map (\(p,(op,v)) -> (p, op ++ showVersion v)) $ filter (not . baselib) alldeps
  where
    alldeps = map convert $ condTreeConstraints $ source
    source  = fromJust $ condLibrary gpkgd
    convert (Dependency (PackageName p) vr) = (p, versionReq vr)

    baselib (p,(o,v)) = any (\(bp,bv) -> bp == p && (op o) bv vb) baseLibs
      where
        vb = versionBranch v
        op "==" = (==)
        op ">=" = (>=)
        op ">"  = (>)
        op _    = (\_ _ -> True)

binaries :: GenericPackageDescription -> [String]
binaries gpkgd = sort $ map fst (condExecutables gpkgd)

versionReq :: VersionRange -> (String,Version)
versionReq (ThisVersion ver)  = ("==", ver)
versionReq (LaterVersion ver) = (">", ver)
versionReq (UnionVersionRanges (ThisVersion v1) (LaterVersion v2))
  | v1 == v2 = (">=", v1)
versionReq (IntersectVersionRanges v _) = versionReq v
versionReq _ = ("", Version [] [])

cabalSetup :: [String] -> [String]
cabalSetup ix
  | any (isSuffixOf "/Setup.lhs") ix  = []
  | otherwise                         = ["", "CABAL_SETUP=\tSetup.hs"]

useHackage :: [(String,String)] -> [String]
useHackage []  = []
useHackage dps = ["", "USE_HACKAGE=\t" ++ (unwords $ map cnv dps)]
  where
    cnv (n,"")  = n
    cnv (n,v)   = n ++ v

uses :: String -> [Dependency] -> Bool
uses tool = any (\(Dependency (PackageName str) _) -> str == tool)

useAlex :: [Dependency] -> [String]
useAlex bt
  | uses "alex" bt = ["USE_ALEX=\tyes"]
  | otherwise      = []

useHappy :: [Dependency] -> [String]
useHappy bt
  | uses "happy" bt = ["USE_HAPPY=\tyes"]
  | otherwise       = []

executable :: [String] -> [String]
executable []   = []
executable exs  = ["", "EXECUTABLE=\t" ++ (unwords exs)]

makefileOf :: GenericPackageDescription -> String -> CalendarTime -> [String] -> String
makefileOf gpkgd category timestamp tgzidx
  = unlines $
    [ "# New ports collection makefile for: " ++ fullNameOf pkgd
    , "# Date created:        " ++ fmt timestamp
    , "# Whom:                haskell@FreeBSD.org"
    , "#"
    , "# $FreeBSD$"
    , "#"
    , ""
    , portname $ nameOf pkgd
    , portversion $ versionOf pkgd
    , categories $ category
    , ""
    , maintainer "haskell@FreeBSD.org"
    , comment $ synopsisOf pkgd
    ] ++
    (cabalSetup tgzidx) ++
    (useHackage $ dependencies gpkgd) ++
    (useAlex $ buildtools gpkgd) ++
    (useHappy $ buildtools gpkgd) ++
    (executable $ binaries gpkgd) ++
    [ ""
    , ".include \"${.CURDIR}/../../lang/ghc/bsd.cabal.mk\""
    , ".include <bsd.port.mk>"
    ]
  where
    pkgd = packageDescription gpkgd

    fmt ts =
      (show $ ctMonth ts) ++ " " ++
      (show $ ctDay ts) ++ ", " ++
      (show $ ctYear ts)

    buildtools
      = buildTools . libBuildInfo . condTreeData . fromJust .  condLibrary


distinfoOf :: PackageDescription -> BS.ByteString -> String
distinfoOf pkgd tgz
  = unlines
    [ "SHA256 (cabal/" ++ tgzName ++ ") = " ++ (show $ sha256 tgz)
    , "SIZE (cabal/" ++ tgzName ++ ") = " ++ (show $ BS.length tgz)
    ]
    where
      tgzName = tarballOf pkgd False

pkgDescrOf :: PackageDescription -> String
pkgDescrOf pkgd
  = unlines $ lines (description pkgd) ++ ["", www url]
    where
      PackageName pn = (pkgName . package) pkgd
      hp = homepage pkgd
      url
        | hp == ""  = "http://hackage.haskell.org/package/" ++ pn
        | otherwise = hp

nameOf :: PackageDescription -> String
nameOf pkgd = name
  where PackageName name = pkgName $ package pkgd

versionOf :: PackageDescription -> String
versionOf pkgd = verstr
  where
    pid     = package pkgd
    version = pkgVersion pid
    verstr  = showVersion version

synopsisOf :: PackageDescription -> String
synopsisOf pkgd = synopsis pkgd

printUsage :: IO ()
printUsage = do
  putStrLn "Usage: hsporter <URL to the .cabal> <category>"
  exitSuccess

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) printUsage
  url <- return $ args !! 0
  category <- return $ args !! 1
  putStrLn $ "Fetching " ++ url ++ "..."
  cabal <- getDescription url
  ParseOk _ gpkg <- return $ parsePackageDescription cabal
  let pkg = packageDescription gpkg
  let tgzUrl = tarballOf pkg True
  putStrLn $ "Fetching " ++ tgzUrl ++ "..."
  tarball <- getTarball tgzUrl
  tgzidx <- return $ tarballIndex tarball
  let dir = category ++ "/" ++ (fullNameOf pkg)
  putStrLn $ "Creating directory " ++ dir ++ "..."
  createDirectoryIfMissing True dir
  putStr "Conversion in progress... ["
  let fileOf f = dir ++ "/" ++ f
  now <- getClockTime
  stamp <- toCalendarTime now
  writeFile (fileOf makefile) (makefileOf gpkg category stamp tgzidx)
  putStr $ sep makefile
  writeFile (fileOf distinfo) (distinfoOf pkg tarball)
  putStr $ sep distinfo
  writeFile (fileOf pkgDescr) (pkgDescrOf pkg)
  putStr $ sep pkgDescr
  putStrLn $ " ]"
  putStrLn $ "Do not forget to do a 'portlint -C'"
  where
    sep str = " " ++ str
