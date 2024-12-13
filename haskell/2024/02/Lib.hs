module Lib where

import Control.Monad
import System.IO

checkIncreasing :: [Int] -> Bool
checkIncreasing [] = True
checkIncreasing (x:[]) = True
checkIncreasing (x1:x2:xs)
  | x1 < x2 = x2 - x1 <= 3 && checkIncreasing (x2 : xs)
  | otherwise = False

checkDecreasing :: [Int] -> Bool
checkDecreasing [] = True
checkDecreasing (x:[]) = True
checkDecreasing (x1:x2:xs)
  | x1 > x2 = x1 - x2 <= 3 && checkDecreasing (x2 : xs)
  | otherwise = False

checkSafe :: [Int] -> Bool
checkSafe [] = True
checkSafe (x:[]) = True
checkSafe (x1:x2:xs)
  | x1 < x2 = x2 - x1 <= 3 && checkIncreasing (x2 : xs)
  | x1 > x2 = x1 - x2 <= 3 && checkDecreasing (x2 : xs)
  | x1 == x2 = False

checkSafeFromString :: String -> Bool
checkSafeFromString = checkSafe . (map read) . words

checkSafeFromLines :: [String] -> Int
checkSafeFromLines [] = 0
checkSafeFromLines (x:xs) =
  case (checkSafeFromString (x)) of
    True -> 1 + checkSafeFromLines (xs)
    False -> checkSafeFromLines (xs)

attemptRemoval :: [Int] -> Int -> Bool
attemptRemoval xs 0 = False
attemptRemoval xs n =
  let partitions = splitAt (n - 1) xs
   in let newList = (fst partitions) ++ (drop 1 $ snd partitions)
       in checkSafe newList || attemptRemoval xs (n - 1)

checkSafeWithRemoval :: [Int] -> Bool
checkSafeWithRemoval xs = (checkSafe xs) || (attemptRemoval xs (length xs))

checkSafeFromStringWithRemoval :: String -> Bool
checkSafeFromStringWithRemoval = checkSafeWithRemoval . (map read) . words

checkSafeFromLinesWithRemoval :: [String] -> Int
checkSafeFromLinesWithRemoval [] = 0
checkSafeFromLinesWithRemoval (x:xs) =
  case (checkSafeFromStringWithRemoval (x)) of
    True -> 1 + checkSafeFromLinesWithRemoval (xs)
    False -> checkSafeFromLinesWithRemoval (xs)
