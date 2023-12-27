{-# LANGUAGE QuasiQuotes #-}

module Y23.D02 (spec2302) where

import Control.Applicative (liftA2)
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.Functor (($>))
import Data.Void (Void)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Text.RawString.QQ qualified as Q

spec2302 :: Spec
spec2302 = describe "Y23D02" $ do
  -- it "1abc2" $ do
  --   M.parse pValue1 "" "1ab2" `shouldParse` Value 1 3
  pure ()

inp1 :: String
inp1 =
  tail . init $
    [Q.r|
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
|]

inp2 :: String
inp2 =
  tail . init $
    [Q.r|
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
|]

main :: IO ()
main = do
  putStrLn "part 1"
  putStrLn "inputs:"
  mapM_ print $ lines inp1

data Cube
  = Red
  | Green
  | Blue
  deriving (Eq, Show)

data Round = Round
  { rRed :: Int,
    rGrn :: Int,
    rBle :: Int
  }

data Game = Game
  { gId :: Int,
    gRounds :: [Round]
  }

type Parser = M.Parsec Void String

spc :: Parser ()
spc = L.space C.space1 M.empty M.empty

lxm :: Parser a -> Parser a
lxm = L.lexeme spc

num :: Parser Int
num = lxm L.decimal

pCube :: Parser Cube
pCube = M.choice $ (\(w, v) -> C.string w $> v) <$> ms
  where
    ms = [("red", Red), ("green", Green), ("blue", Blue)]

pRound = c `M.sepBy` lxm (C.char ',')
  where
    c = (,) <$> num <*> pCube
