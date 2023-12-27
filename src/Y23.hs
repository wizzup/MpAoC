module Y23 (spec23) where

import Y23.D01 (spec2301)
import Y23.D02 (spec2302)
import Test.Hspec

spec23 :: Spec
spec23 = do
  spec2301
  spec2302
