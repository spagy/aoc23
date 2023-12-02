#! /usr/bin/env runghc

import Data.Char
import Data.Maybe
import Data.List

replacements :: [(String, Char)]
replacements =
  [ ("one", '1')
  , ("two", '2')
  , ("three", '3')
  , ("four", '4')
  , ("five", '5')
  , ("six", '6')
  , ("seven", '7')
  , ("eight", '8')
  , ("nine", '9')
  ]

getStartingDigit :: String -> Maybe Char
getStartingDigit "" = Nothing
getStartingDigit line@(c : _)
  | isDigit c = Just c
  | otherwise = snd <$> find isLinePrefix replacements
  where
    isLinePrefix (word, _) = word `isPrefixOf` line

lineToDigits :: String -> [Char]
lineToDigits line
  = mapMaybe getStartingDigit (tails line)

lineToNumber :: String -> Int
lineToNumber line =
  let digits = lineToDigits line
   in read [head digits, last digits]

run :: String -> Int
run input =
  let rows = lines input
      nums = lineToNumber <$> rows
   in sum nums

wrap :: Show a => (String -> a) -> (String -> String)
wrap f input
  = show (f input) <> "\n"

main :: IO ()
main = interact (wrap run)
