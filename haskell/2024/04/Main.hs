module Main where

import System.IO
import Control.Monad
import Lib

main = do
        contents <- readFile "input"
        print $ countXmas $ contents
        print $ countCrossMas $ contents
