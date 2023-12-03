{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor qualified as B
import Data.Char (isDigit)
import Data.Either qualified as E
import Data.Text (splitOn)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read qualified as T
import Data.Tuple

data Color = Red | Green | Blue deriving (Show)

getCubeColor col
  | T.isInfixOf "red" col = Red
  | T.isInfixOf "green" col = Green
  | T.isInfixOf "blue" col = Blue
  | otherwise = undefined

-- processSets str = map (getCubeCount . splitOn ",") $ splitOn ";" str

getCubeCount str = swap . B.second getCubeColor . E.fromRight (0, "") . T.decimal $ T.strip str

getID :: T.Text -> Int
getID str = fst $ E.fromRight (0, "") $ T.decimal (T.filter isDigit str)

-- splitInput = map ((\x -> (getID $ head x, processSets (head $ tail x))) . splitOn ":")

parseInput :: (Num a) => [[(Color, a)]]
parseInput =
  undefined

readInput = T.lines <$> T.readFile "day_02_sample.txt"

main :: IO ()
main = do
  input <- T.lines <$> T.readFile "day_02_sample.txt"
  -- let processed = splitInput input
  T.putStrLn "not implemented"