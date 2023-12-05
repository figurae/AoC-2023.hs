{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor qualified as B
import Data.Char
import Data.Either qualified as E
import Data.Function
import Data.List
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read qualified as T
import Data.Tuple

data Color = Red | Green | Blue deriving (Eq, Ord, Show)

type Set = (Color, Int)

type Game = (Int, [[Set]])

maxRedCubes = 12

maxGreenCubes = 13

maxBlueCubes = 14

getCubeColor :: T.Text -> Color
getCubeColor col
  | T.isInfixOf "red" col = Red
  | T.isInfixOf "green" col = Green
  | T.isInfixOf "blue" col = Blue
  | otherwise = undefined

processSets :: T.Text -> [[Set]]
processSets = map (map getCubeCount . T.splitOn ",") . T.splitOn ";"

getCubeCount :: T.Text -> Set
getCubeCount = swap . B.second getCubeColor . E.fromRight (0, "") . T.decimal . T.strip

getId :: T.Text -> Int
getId = fst . E.fromRight (0, "") . T.decimal . T.filter isDigit

processInput :: [T.Text] -> [Game]
processInput = map $ (\x -> (getId $ head x, processSets $ head $ tail x)) . T.splitOn ":"

isSetPossible :: Int -> Int -> Int -> Set -> Bool
isSetPossible rMax gMax bMax (color, num) =
  case color of
    Red -> num <= rMax
    Green -> num <= gMax
    Blue -> num <= bMax

getMaxCubes :: Set -> Set -> Set
getMaxCubes (color1, num1) (color2, num2)
  | color1 == color2 = (color1, max num1 num2)
  | otherwise = undefined

reduceSets :: [[Set]] -> [Set]
reduceSets = map (foldl1' getMaxCubes) . groupBy ((==) `on` fst) . sort . concat

areSetsPossible :: [Game] -> [(Int, Bool)]
areSetsPossible =
  map (B.second (all (all (isSetPossible maxRedCubes maxGreenCubes maxBlueCubes))))

addPossibleIds :: [(Int, Bool)] -> Int
addPossibleIds = sum . map fst . filter snd

readInput :: IO [T.Text]
readInput = T.lines <$> T.readFile "day_02_input.txt"

main :: IO ()
main = do
  input <- readInput
  let processedInput = processInput input
  let part1 = addPossibleIds . areSetsPossible $ processedInput
  let part2 = sum . map (product . map snd . reduceSets . snd) $ processedInput
  putStr "Part 1: "
  print part1
  putStr "Part 2: "
  print part2
