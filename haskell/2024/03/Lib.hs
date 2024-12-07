module Lib where

import System.IO
import Control.Monad
import Text.Regex.PCRE

findMatches :: String -> [[String]]
findMatches = flip (=~) "mul\\(([0-9]+),([0-9]+)\\)"

sumMatches :: [[String]] -> Int
sumMatches = sum . map (product . map (read :: String -> Int) . drop 1)

findEnabledMatches :: String -> [[String]]
findEnabledMatches input = findMatches $ join $ map join (("do()" ++ input ++ "don't()") =~ "do\\(\\).*?don't\\(\\)" :: [[String]])

