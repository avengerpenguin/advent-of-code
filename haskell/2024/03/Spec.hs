module Main where

import Test.Hspec
import Lib


main :: IO ()
main = hspec $ do
    describe "findMatches" $ do
        it "find matches" $ do
            findMatches "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))" `shouldBe` [["mul(2,4)","2","4"],["mul(5,5)","5","5"],["mul(11,8)","11","8"],["mul(8,5)","8","5"]]


    describe "sumMatches" $ do
        it "sums matches" $ do
            sumMatches [["mul(2,4)","2","4"],["mul(5,5)","5","5"],["mul(11,8)","11","8"],["mul(8,5)","8","5"]] `shouldBe` 161


    describe "findEnabledMatches" $ do
        it "finds enabled matches only" $ do
            findEnabledMatches "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))" `shouldBe` [["mul(2,4)","2","4"],["mul(8,5)","8","5"]]

        it "sums only enabled matches" $ do
            (sumMatches . findEnabledMatches) "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))" `shouldBe` 48
