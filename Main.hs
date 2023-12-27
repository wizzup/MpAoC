module Main where

import Y15 (spec15)
import Y23 (spec23)
import Test.Hspec

main :: IO ()
main = hspec $ do
  spec15
  spec23

