module Lib where

import System.IO
import Control.Monad

checkIncreasing :: [Int] -> Bool
checkIncreasing [] = True
checkIncreasing (x:[]) = True
checkIncreasing (x1:x2:xs)
 | x1 < x2      = x2 - x1 <= 3 && checkIncreasing (x2:xs)
 | otherwise    = False

checkDecreasing :: [Int] -> Bool
checkDecreasing [] = True
checkDecreasing (x:[]) = True
checkDecreasing (x1:x2:xs)
 | x1 > x2      = x1 - x2 <= 3 && checkDecreasing (x2:xs)
 | otherwise    = False

checkSafe :: [Int] -> Bool
checkSafe [] = True
checkSafe (x:[]) = True
checkSafe (x1:x2:xs)
    | x1 < x2   = x2 - x1 <= 3 && checkIncreasing (x2:xs)
    | x1 > x2   = x1 - x2 <= 3 && checkDecreasing (x2:xs)
    | x1 == x2  = False

checkSafeFromString :: String -> Bool
checkSafeFromString = checkSafe . (map read) . words

checkSafeFromLines :: [String] -> Int
checkSafeFromLines [] = 0
checkSafeFromLines (x:xs) = case (checkSafeFromString(x)) of
    True    -> 1 + checkSafeFromLines(xs)
    False   ->  checkSafeFromLines(xs)
