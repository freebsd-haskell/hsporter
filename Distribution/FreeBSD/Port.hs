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

module Distribution.FreeBSD.Port where

import Codec.Archive.Tar as Tar
import Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Applicative
import Control.Arrow
import Data.Char
import Data.Digest.Pure.SHA
import Data.Function
import Data.List
import qualified Data.Map as DM
import Data.Maybe
import qualified Data.Text as DT
import Data.Version
import Distribution.FreeBSD.Common
import Distribution.License
import Distribution.ModuleName hiding (main)
import Distribution.Package
import Distribution.PackageDescription hiding (maintainer, category, license)
import qualified Distribution.PackageDescription as DP
import Distribution.PackageDescription.Parse
import Distribution.Version
import Network.HTTP
import Network.URI
import System.FilePath.Posix
import System.IO
import System.Time
import Text.ParserCombinators.ReadP

withText :: (DT.Text -> DT.Text) -> (String -> String)
withText f = DT.unpack . f . DT.pack

format :: Int -> Maybe Char -> [String] -> [String]
format w t =
  map DT.unpack . reverse . uncurry (:) .
  (head &&& (map (flip DT.append term) . tail)) .
  reverse .  map stuff . cut . map DT.pack
  where
    cut [] = []
    cut ws = i : cut (drop (length i) ws)
      where i = ideal ws

    (width,term) =
      case t of
        Just x  -> (w - 2, DT.pack (' ':x:[]))
        Nothing -> (w, DT.pack "")

    ideal     = minimumBy (compare `on` (dist width . estimate)) . inits
    estimate  = DT.length . stuff
    stuff     = DT.intercalate (DT.pack " ")
    dist x y  = abs (x - y)

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
        then hackageURI ++ nameOf pkgd ++ "/" ++ versionOf pkgd ++ "/"
        else ""

tgzEntries :: BS.ByteString -> Entries
tgzEntries = Tar.read . GZip.decompress

tarballIndex :: BS.ByteString -> [String]
tarballIndex tgz = foldEntries translate [] (\_ -> []) entries
  where
    translate entry ls = (entryPath entry):ls
    entries = tgzEntries tgz

licenseText :: BS.ByteString -> FilePath -> String
licenseText tgz name
  | not . null $ files = BS.unpack $ head files
  | otherwise = ""
  where
    entries = tgzEntries tgz
    files   = foldEntries find [] (const []) entries
    find x ls  =
      if ((takeBaseName $ entryPath x) == name)
        then cts:ls
        else ls
          where
            NormalFile cts _ = entryContent x

www :: String -> String
www url = "WWW:\t" ++ url

maintainer :: String -> String
maintainer m = "MAINTAINER=\t" ++ m

comment :: String -> String
comment = withText $
  DT.append (DT.pack "COMMENT=\t") .
  DT.dropWhileEnd (== '.')

portname :: String -> String
portname n = "PORTNAME=\t" ++ n

portversion :: String -> String
portversion v = "PORTVERSION=\t" ++ v

categories :: String -> String
categories c = "CATEGORIES=\t" ++ cats
  where cats = foldl1 (++) $ intersperse " " $ [c] ++ ["haskell"]

findLicense :: License -> Maybe String
findLicense (GPL _)  = Just "GPLv"
findLicense (LGPL _) = Just "LGPL21"
findLicense BSD3     = Just "BSD"
findLicense BSD4     = Just "BSD"
findLicense MIT      = Just "MIT"
findLicense _        = Nothing

findGPLVersion :: String -> String
findGPLVersion =
  DT.unpack .
  DT.take 1 . (!! 1) .
  DT.words .
  snd . DT.breakOn (DT.toUpper $ DT.pack "Version ") .
  DT.toUpper . DT.pack

license :: (License,FilePath) -> String -> [String]
license (l,lf) lictxt =
  case (findLicense l) of
    Just lic@"GPLv" -> renderLicense (lic ++ findGPLVersion lictxt)
    Just lic -> renderLicense lic
    Nothing  -> []
  where
    renderLicense l =
      ["LICENSE=\t" ++ l] ++ setFile lf
      where
        setFile ""        = []
        setFile "LICENSE" = []
        setFile other     = ["FILE_LICENSE=\t" ++ other]

prefix :: String
prefix = "hs-"

normalize :: [String] -> [String]
normalize = filter nonEmpty . map (trim . uncomment)
  where
    nonEmpty = not . null
    uncomment = takeWhile (/= '#')
    trim = f . f
      where f = reverse . dropWhile isSpace

getBaseLibs :: FilePath -> IO [(String,[Int])]
getBaseLibs coreConf = do
  contents <- lines <$> readFile coreConf
  return $ sort $ translate <$> normalize contents
  where
    translate = name &&& version
      where
        name = takeWhile (not . isSpace)
        version =
          versionBranch . fst . last .
          (readP_to_S parseVersion) .
          dropWhile isSpace .
          dropWhile (not . isSpace)

splitBy b = map (DT.unpack . DT.strip) . DT.split b . DT.pack

getCategoryMap :: FilePath -> IO (DM.Map String String)
getCategoryMap path = do
  contents <- (normalize . lines) <$> readFile path
  return $ DM.fromList $ translate <$> contents
  where
    translate str = (a,b)
      where a:b:_ = splitBy (== ':') str

findCategory :: DM.Map String String -> String -> String
findCategory cmap c =
  snd $ maximumBy (compare `on` fst) $ distrib $ find <$> cats
  where
    cats = splitBy (== ',') c
    find x = DM.findWithDefault "misc" x cmap
    distrib = map (length &&& head) . group . sort

defaultFlags = map flagName . filter flagDefault . genPackageFlags

active gpkgd (Var (Flag f)) = f `elem` (defaultFlags gpkgd)
active _ _                  = False

dependencies :: [(String,[Int])] -> GenericPackageDescription -> [(String,String)]
dependencies baseLibs gpkgd =
  sortBy (compare `on` (map toUpper . fst)) .
  map (\(p,(op,v)) -> (p, op ++ showVersion v)) .
  filter (not . baselib) .
  map convert $
  collectDeps
  where
    convert (Dependency (PackageName p) vr) = (p, versionReq vr)

    baselib (p,(o,v)) = any (\(bp,bv) -> bp == p && (op o) bv vb) baseLibs
      where
        vb = versionBranch v
        op "==" = (==)
        op ">=" = (>=)
        op ">"  = (>)
        op _    = (\_ _ -> True)

    collectDeps = concat [libdeps,exedeps]
      where
        libdeps   =
          case (condLibrary gpkgd) of
            Just x  -> findDeps x
            Nothing -> []
        exedeps   = concatMap (findDeps . snd) . condExecutables $ gpkgd

    findDeps = uncurry (++) .
      ((condTreeConstraints) &&&
      (concatMap condTreeConstraints . catMaybes . map pick . condTreeComponents))

    pick (opt,pri,sec)
      | active gpkgd opt  = Just pri
      | otherwise         = sec

binaries :: GenericPackageDescription -> [String]
binaries gpkgd = sort . map fst . filter enabled . condExecutables $ gpkgd
  where
    enabled (_,c)
      | null conds  = True
      | otherwise   = or conds
      where conds = map f . condTreeComponents $ c

    f (opt,d,_) = (active gpkgd opt) && included d
      where included = buildable . buildInfo . condTreeData

standalone :: GenericPackageDescription -> [String]
standalone gpkgd
  | isNothing (condLibrary gpkgd) = ["", "STANDALONE=\tyes"]
  | otherwise = []

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
useHackage dps =
  ["", prologue ++ head packs] ++ map ("\t\t" ++) (tail packs)
  where
    prologue = "USE_CABAL=\t"
    packs    = format 56 (Just '\\') $ map cnv dps

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

useC2hs :: [Dependency] -> [String]
useC2hs bt
  | uses "c2hs" bt  = ["USE_C2HS=\tyes"]
  | otherwise       = []

executable :: [String] -> [String]
executable []   = []
executable exs  = ["", "EXECUTABLE=\t" ++ (unwords exs)]

makefileOf :: [(String,[Int])] -> String -> GenericPackageDescription -> String -> CalendarTime -> [String] -> String
makefileOf baseLibs lictxt gpkgd category timestamp tgzidx
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
    , ""
    ] ++
    (license (licensingOf pkgd) lictxt) ++
    (cabalSetup tgzidx) ++
    (useHackage $ dependencies baseLibs gpkgd) ++
    (useAlex $ buildtools gpkgd) ++
    (useHappy $ buildtools gpkgd) ++
    (useC2hs $ buildtools gpkgd) ++
    (executable $ binaries gpkgd) ++
    (standalone gpkgd) ++
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

    buildtools gpkgd =
      concatMap buildTools $ concat [libTools,exeTools]
      where
        libTools  =
          case (condLibrary gpkgd) of
            Just x  -> [libBuildInfo $ condTreeData x]
            Nothing -> []

        exeTools  =
          map (buildInfo . condTreeData . snd) . condExecutables $ gpkgd


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
  = unlines $ (format 72 Nothing . words $ contents) ++ ["", www url]
    where
      desc = description pkgd
      contents
        | null desc = synopsis pkgd
        | otherwise = desc

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

licensingOf :: PackageDescription -> (License,FilePath)
licensingOf = DP.license &&& DP.licenseFile

data BuildOpts = BuildOpts {
    baseLibConf     :: FilePath
  , categoriesConf  :: FilePath
  }

data Port = Port {
    makefile :: String
  , distinfo :: String
  , pkgDescr :: String
  }

buildPort :: BuildOpts -> String -> Maybe String -> IO (FilePath,Port)
buildPort opts dump Nothing = do
  catMap <- getCategoryMap $ categoriesConf opts
  ParseOk _ gpkg <- return $ parsePackageDescription dump
  let pkg = packageDescription gpkg
  let category = findCategory catMap (DP.category pkg)
  buildPort opts dump (Just category)

buildPort opts dump (Just category) = do
  baseLibs <- getBaseLibs $ baseLibConf opts
  ParseOk _ gpkg <- return $ parsePackageDescription dump
  let pkg = packageDescription gpkg
  let tgzUrl = tarballOf pkg True
  tarball <- getTarball tgzUrl
  tgzidx <- return $ tarballIndex tarball
  lic <- return $ licenseText tarball (DP.licenseFile pkg)
  now <- getClockTime
  stamp <- toCalendarTime now
  return $
    (category </> fullNameOf pkg
    ,Port
      (makefileOf baseLibs lic gpkg category stamp tgzidx)
      (distinfoOf pkg tarball)
      (pkgDescrOf pkg))
