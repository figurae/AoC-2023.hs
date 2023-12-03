{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.Text (Text, filter, head, last, lines, replace)
import Data.Text.IO (readFile)
import Prelude hiding (filter, head, last, lines, readFile)

-- NOTE: this is cheating a bit in order to handle overlapping cases, but hey, it works
wordsToDigits :: Text -> Text
wordsToDigits =
  replace "one" "on1ne"
    . replace "two" "tw2wo"
    . replace "three" "thre3hree"
    . replace "four" "fou4our"
    . replace "five" "fiv5ive"
    . replace "six" "si6ix"
    . replace "seven" "seve7even"
    . replace "eight" "eigh8ight"
    . replace "nine" "nin9ine"

getDigitsFromLine :: Text -> Text
getDigitsFromLine = filter isDigit

combineFirstLastDigit :: Text -> Int
combineFirstLastDigit s = read [head s, last s]

processInput1 :: Text -> String
processInput1 input = show . sum . map (combineFirstLastDigit . getDigitsFromLine) $ lines input

processInput2 :: Text -> String
processInput2 input = show . sum . map (combineFirstLastDigit . getDigitsFromLine . wordsToDigits) $ lines input

main :: IO ()
main = do
  input <- readFile "day_01_input.txt"
  putStr "Part 1: "
  putStrLn $ processInput1 input
  putStr "Part 2: "
  putStrLn $ processInput2 input