module Y15.D03 (spec1503) where

import Data.Functor (($>))
import Data.Void (Void)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (Parsec, choice, many, parse)
import Text.Megaparsec.Char (char)

spec1503 :: Spec
spec1503 = describe "Y15D03" $ do
  it ">" $ do
    parse pDirs "" ">" `shouldParse` [East]
  it ">^v<" $ do
    parse pDirs "" ">^v<" `shouldParse` [East, North, South, West]

type Parser = Parsec Void String

data Dir = North | South | East | West deriving (Show, Eq)

pDirs :: Parser [Dir]
pDirs = many $ choice cs
  where
    cs = map (\(c, d) -> char c $> d) [('^', North), ('v', South), ('>', East), ('<', West)]
