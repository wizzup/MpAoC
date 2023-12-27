module Y23.D01 (spec2301) where

import Control.Applicative (liftA2)
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.Functor (($>))
import Data.Void (Void)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as C
import Test.Hspec (Spec, it, describe)
import Test.Hspec.Megaparsec (shouldParse)

spec2301 :: Spec
spec2301 = describe "Y23D01" $ do
  it "1abc2" $ do
    M.parse pValue1 "" "1ab2" `shouldParse` Value 1 2
  it "two1nine -- fwd" $ do
    M.parse pValue2F "" "two1nine" `shouldParse` 2
  it "two1nine -- rwd" $ do
    M.parse pValue2B "" (reverse "two1nine") `shouldParse` 9

type Parser = M.Parsec Void String

data Value = Value {firstDigit :: Int, lastDigit :: Int} deriving (Show, Eq)

pSingleDigit :: Parser Int
pSingleDigit = read . (: []) <$> C.digitChar

skipNonDigit :: Parser ()
skipNonDigit = M.skipMany $ M.satisfy (not . isDigit)

-- for part 1
pValue1 :: Parser Value
pValue1 = do
  ns <- M.many (skipNonDigit *> pSingleDigit <* skipNonDigit)
  M.eof
  pure $ Value (head ns) (last ns)

numWords :: [(String, Int)]
numWords =
  [ ("zero", 0),
    ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9)
  ]

numWordsB :: [(String, Int)]
numWordsB = first reverse <$> numWords

pNumWord :: Parser Int
pNumWord = M.choice $ (\(w, i) -> C.string w $> i) <$> numWords

pNumWordB :: Parser Int
pNumWordB = M.choice $ (\(w, i) -> C.string w $> i) <$> numWordsB

-- for part 2
pValue2F :: Parser Int
pValue2F = C.letterChar `M.skipManyTill` (pSingleDigit M.<|> pNumWord)

pValue2B :: Parser Int
pValue2B = C.letterChar `M.skipManyTill` (pSingleDigit M.<|> pNumWordB)
