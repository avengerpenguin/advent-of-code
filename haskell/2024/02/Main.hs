module Main where

import System.IO
import Lib

main = do
        contents <- readFile "input"
        print $ checkSafeFromLines $ lines contents
        print $ checkSafeFromLinesWithRemoval $ lines contents
