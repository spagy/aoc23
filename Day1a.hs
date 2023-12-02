#! /usr/bin/env runghc

import Data.Char

lineToNumber :: String -> Int
lineToNumber line =
  let digits = filter isDigit line
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
