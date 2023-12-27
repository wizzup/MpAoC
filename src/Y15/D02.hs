module Y15.D02 (spec1502) where

import Data.Void (Void)
import Text.Megaparsec (Parsec, parse)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldParse)

spec1502 :: Spec
spec1502 = describe "Y15D02" $ do
  it "2x3x4" $ do
    parse pDimen "" "2x3x4" `shouldParse` Dimen 2 3 4
  it "1x1x10" $ do
    parse pDimen "" "1x1x10" `shouldParse` Dimen 1 1 10

data Dimen = Dimen Int Int Int deriving (Show, Eq)

type Parser = Parsec Void String

pDimen :: Parser Dimen
pDimen = Dimen <$> (decimal <* char 'x') <*> (decimal <* char 'x') <*> decimal
