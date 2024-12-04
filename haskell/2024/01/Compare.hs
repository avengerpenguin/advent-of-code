{-# LANGUAGE TupleSections #-}

module Compare where

import System.IO
import Control.Monad
import Data.List
import Data.Tuple.Extra
import qualified Data.Map.Strict as Map


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

countList :: [Int] -> Map.Map Int Int
countList = Map.fromListWith (+) . map (,1)

calculateSimilarity :: Map.Map Int Int -> [Int] -> Int
calculateSimilarity _ [] = 0
calculateSimilarity counts (x:xs) = (x * Map.findWithDefault 0 x counts) + calculateSimilarity counts xs

calculateSimilarityFromString :: String -> Int
calculateSimilarityFromString input = let lists = parseLists input
                                in calculateSimilarity (countList (snd lists)) (fst lists)
