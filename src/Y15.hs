module Y15 (spec15) where

import Y15.D01 (spec1501)
import Y15.D02 (spec1502)
import Y15.D03 (spec1503)
import Test.Hspec

spec15 :: Spec
spec15 = do
  spec1501
  spec1502
  spec1503
