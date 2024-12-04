module Main where

import System.IO
import Compare

main = do
        contents <- readFile "input"
        print $ compareDistance contents
