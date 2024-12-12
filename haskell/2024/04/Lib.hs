module Lib where

import System.IO
import Control.Monad
import Text.Printf
import Data.List
import Data.List.Unique
import Text.Regex.PCRE
import qualified Data.Text as T

rows :: String -> [String]
rows = lines

cols :: String -> [String]
cols = map join . transpose . map (map (:[])) . lines

mapIndex :: (a -> Int -> b) -> [a] -> [b]
mapIndex f l = map (uncurry f) $ zip l [0..length l - 1]

diagLeft :: String -> [String]
diagLeft s = (transpose $ mapIndex (flip drop) (rows s)) ++ drop 1 (transpose $ mapIndex (flip drop) (reverse (rows s)))

diagRight :: String -> [String]
diagRight s = (transpose $ mapIndex (flip drop) (map reverse (rows s))) ++ drop 1 (transpose $ mapIndex (flip drop) (map reverse (reverse (rows s))))

findMatches :: String -> [[String]]
findMatches s = s =~ "XMAS" ++ s =~ "SAMX"

countXmas :: String -> Int
countXmas s = let strings = rows s ++ cols s ++ diagLeft s ++ diagRight s
                in sum $ map (length . findMatches) strings

grid :: String -> [[String]]
grid s = map (map (:[])) $ rows s

--findOverlappingMatches :: String -> String -> [String]
--findOverlappingMatches regex input = go input
--  where
--    go str
--        | T.null (T.pack str) = [] -- Stop when the string is empty
--        | otherwise =
--            case (str =~ regex :: (String, String, String, [String])) of
--                (_, _, _, []) -> [] -- No match found
--                (_, match, _:rest, _) ->
--                    match : go (tail (match ++ rest))
--
--_countCrossMas :: String -> (String, String) -> Int
--_countCrossMas s (top, bottom) = let width = length . head . lines $ s
--                    in let regex = printf "(?s)(?=(%s.{%s}A.{%s}%s))" top (show $ width - 1) (show $ width - 1) bottom :: String
--                        in length $ findOverlappingMatches regex s
--
countCrossMas :: String -> Int
countCrossMas s = sum $ map (_countCrossMas s) [
                    ("S.M", "S.M")
                  , ("S.S", "M.M")
                  , ("M.M", "S.S")
                  , ("M.S", "M.S")
                ]


--countCrossMas :: [[String]] -> Int
--countCrossMas (("M":x2:"M":xs) : (y1:"A":y3:ys): ("S":z2:"S":zs): rest) = 1 + countCrossMas ((x2:"M":xs) : ("A":y3:ys): (z2:"S":zs): []) + countCrossMas ((y1:"A":y3:ys): (z2:"S":zs): rest)
--countCrossMas (("M":x2:"S":xs) : (y1:"A":y3:ys): ("M":z2:"S":zs): rest) = 1 + countCrossMas ((x2:"S":xs) : ("A":y3:ys): (z2:"S":zs): []) + countCrossMas ((y1:"A":y3:ys): (z2:"S":zs): rest)
--countCrossMas (("S":x2:"M":xs) : (y1:"A":y3:ys): ("S":z2:"M":zs): rest) = 1 + countCrossMas ((x2:"M":xs) : ("A":y3:ys): (z2:"M":zs): []) + countCrossMas ((y1:"A":y3:ys): (z2:"M":zs): rest)
--countCrossMas (("S":x2:"S":xs) : (y1:"A":y3:ys): ("M":z2:"M":zs): rest) = 1 + countCrossMas ((x2:"S":xs) : ("A":y3:ys): (z2:"M":zs): []) + countCrossMas ((y1:"A":y3:ys): (z2:"M":zs): rest)
--countCrossMas ((x1:x2:x3:xs) : (y1:y2:y3:ys): (z1:z2:z3:zs): rest) = countCrossMas ((x2:x3:xs) : (y2:y3:ys): (z2:z3:zs): rest) + countCrossMas ((y1:y2:y3:ys): (z1:z2:z3:zs): rest)
--countCrossMas _ = 0

_windows :: [[String]] -> [String]
_windows ((x1:x2:x3:xs) : (y1:y2:y3:ys) : (z1:z2:z3:zs): rest) = [x1 ++ x2 ++ x3 ++ y1 ++ y2 ++ y3 ++ z1 ++ z2 ++ z3] ++ _windows ((x2:x3:xs) : (y2:y3:ys): (z2:z3:zs): []) ++ _windows ((y1:y2:y3:ys): (z1:z2:z3:zs): rest)
_windows _ = []

windows :: String -> [String]
windows s = _windows $ map (map (:[])) $ rows s

_countCrossMas :: String -> (String, String) -> Int
_countCrossMas s (top, bottom) = let regex = printf "%s.A.%s" top bottom :: String
                                    in length $ filter (\w -> (w =~ regex :: Bool)) $ windows s

--testGrid =  "MMMSXXMASM\n\
--            \MSAMXMSMSA\n\
--            \AMXSXMAAMM\n\
--            \MSAMASMSMX\n\
--            \XMASAMXAMM\n\
--            \XXAMMXXAMA\n\
--            \SMSMSASXSS\n\
--            \SAXAMASAAA\n\
--            \MAMMMXMMMM\n\
--            \MXMXAXMASX"
