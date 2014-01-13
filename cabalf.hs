{-# LANGUAGE TupleSections #-}
-- get code from hackage (github, darcs hub)
--
-- frequencies of given extensions
--
-- frequencies that extensions occur together?
--
-- ==> make better suggestions
import Control.Applicative
import Data.Char
import Text.HTML.TagSoup
import Network.Curl
import System.IO
import Data.List.Split
import Data.List
import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Monoid
import System.Cmd
import System.IO.Temp
import System.Directory
import System.FilePath
import System.Process
import DynFlags
import System.Environment

main2 = do
    f <- fmap parseTags (readFile "index.html")
    print [ p | TagOpen "a" [("href", p)] <- f ]

main = cachedPkgs
cachedPkgs = do
    home <- getEnv "HOME"
    cs <- readProcess "find" [home </> ".cabal/packages/hackage.haskell.org/",
                              "-name", "*.tar.gz",
                              "!", "-name", "00-index.tar.gz"] ""
    d0 <- getCurrentDirectory
    pkgs <- withTempDirectory "/tmp" "pkg." $ \tmp -> do
        setCurrentDirectory tmp
        mapM (getLangsPkg tmp) $ lines cs
    setCurrentDirectory d0
    writeFile "pp.csv" $
        ("pkg,fileid,ext,count\n"++) $
        concatMap pp $ lines cs `zip` map snd pkgs

pp :: (String, [M.Map String (Sum Int)]) -> String
pp (pkg, counts) = concat $ zipWith f counts [1 .. ]
    where
    f m fileid = concatMap (\ (c, Sum n) ->
        (takeBaseName pkg ++ "," ++ show fileid ++ "," ++ c ++ "," ++ show n ++ "\n") )
        (M.toList  m)
getLangsPkg tmp tar = do
    copyFile tar (tmp </> takeBaseName tar)
    system ("tar xf "++ takeBaseName tar)
    fs <- readProcess "find" ["-regextype","posix-extended",
                              "-regex",".*(lhs|hs|cabal)"] ""
    r <- forM (lines fs) $ \f -> do
        fn <- case takeExtension f of
                ".cabal" -> getLangC <$> readFile f
                ".hs" -> getLangF <$> readFile f
                ".lhs" -> getLangF <$> readFile f
                _ -> return mempty
        let m = M.fromListWith (<>) (map (, Sum (1::Int)) fn)
        return $! m
    return (M.unionsWith (<>) r, r)

xS :: M.Map String String
xS = M.fromList $ map (\(x,_,_) -> (map toLower x, x)) xFlags

getLangC :: String -> [String]
getLangC = mapMaybe (`M.lookup` xS)
    . concatMap (split (dropDelims $ condense $ oneOf " \n,"))
    . mapMaybe (listToMaybe . splitOn ":" <=< stripPrefix "extensions:")
    . split (keepDelimsL (onSublist "extensions:"))
    . map toLower

getLangF :: String -> [String]
getLangF c = mapMaybe (`M.lookup` xS) $ map (map toLower) $
    concatMap (split (dropDelims $ condense $ oneOf " ,\t")) $
    mapMaybe (listToMaybe . splitOn " #-}" <=< stripPrefix "{-# LANGUAGE ") $
    split (keepDelimsL (onSublist "{-# LANGUAGE")) $
    unwords $
    takeWhile (not . isPrefixOf "module") $
    lines c
