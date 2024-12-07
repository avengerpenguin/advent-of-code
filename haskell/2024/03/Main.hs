module Main where

import System.IO
import Control.Monad
import Lib

main = do
        contents <- readFile "input"
        print $ sumMatches . findMatches . join . lines $ contents
        print $ sumMatches . findEnabledMatches . join . lines $ contents
