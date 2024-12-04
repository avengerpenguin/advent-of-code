module Compare where

import System.IO
import Control.Monad
import Data.List
import Data.Tuple.Extra

readInt :: String -> Int
readInt = read

tuple2 :: [a] -> (a, a)
tuple2 (x:y:xs) = (x, y)

parseRow :: String -> (Int, Int)
parseRow row = tuple2 $ map readInt $ words row

parseLists :: String -> ([Int], [Int])
parseLists input = unzip $ map parseRow $ lines input

distance :: ([Int], [Int]) -> Int
distance ([], []) = 0
distance (x:xs, y:ys) = abs (x - y) + distance(xs, ys)

compareDistance :: String -> Int
compareDistance input = distance $ both sort $ parseLists input
