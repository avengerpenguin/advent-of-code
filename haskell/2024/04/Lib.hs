module Lib where

import Control.Monad
import Data.List
import Data.List.Unique
import qualified Data.Text as T
import System.IO
import Text.Printf
import Text.Regex.PCRE

rows :: String -> [String]
rows = lines

cols :: String -> [String]
cols = map join . transpose . map (map (: [])) . lines

mapIndex :: (a -> Int -> b) -> [a] -> [b]
mapIndex f l = map (uncurry f) $ zip l [0 .. length l - 1]

diagLeft :: String -> [String]
diagLeft s =
  (transpose $ mapIndex (flip drop) (rows s))
    ++ drop 1 (transpose $ mapIndex (flip drop) (reverse (rows s)))

diagRight :: String -> [String]
diagRight s =
  (transpose $ mapIndex (flip drop) (map reverse (rows s)))
    ++ drop
         1
         (transpose $ mapIndex (flip drop) (map reverse (reverse (rows s))))

findMatches :: String -> [[String]]
findMatches s = s =~ "XMAS" ++ s =~ "SAMX"

countXmas :: String -> Int
countXmas s =
  let strings = rows s ++ cols s ++ diagLeft s ++ diagRight s
   in sum $ map (length . findMatches) strings

grid :: String -> [[String]]
grid s = map (map (: [])) $ rows s

countCrossMas :: String -> Int
countCrossMas s =
  sum
    $ map
        (_countCrossMas s)
        [("S.M", "S.M"), ("S.S", "M.M"), ("M.M", "S.S"), ("M.S", "M.S")]

_windows :: [[String]] -> [String]
_windows ((x1:x2:x3:xs):(y1:y2:y3:ys):(z1:z2:z3:zs):rest) =
  [x1 ++ x2 ++ x3 ++ y1 ++ y2 ++ y3 ++ z1 ++ z2 ++ z3]
    ++ _windows ((x2 : x3 : xs) : (y2 : y3 : ys) : (z2 : z3 : zs) : [])
    ++ _windows ((y1 : y2 : y3 : ys) : (z1 : z2 : z3 : zs) : rest)
_windows _ = []

windows :: String -> [String]
windows s = _windows $ map (map (: [])) $ rows s

_countCrossMas :: String -> (String, String) -> Int
_countCrossMas s (top, bottom) =
  let regex = printf "%s.A.%s" top bottom :: String
   in length $ filter (\w -> (w =~ regex :: Bool)) $ windows s
