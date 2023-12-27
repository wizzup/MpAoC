module Y15.D01 (spec1501) where

import Data.Functor (($>))
import Data.Void (Void)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (Parsec, choice, many, parse)
import Text.Megaparsec.Char (char)

spec1501 :: Spec
spec1501 = describe "Y15D01" $ do
  it "(())" $ do
    parse pParens "" "(())" `shouldParse` [Open, Open, Close, Close]
  it "()()" $ do
    parse pParens "" "()()" `shouldParse` [Open, Close, Open, Close]

data Paren
  = Open
  | Close
  deriving (Show, Eq)

type Parser = Parsec Void String

pOpen :: Parser Paren
pOpen = char '(' $> Open

pClose :: Parser Paren
pClose = char ')' $> Close

pParens :: Parser [Paren]
pParens = many $ choice [pOpen, pClose]
