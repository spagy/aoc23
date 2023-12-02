#!/usr/bin/env runghc

import Day2.Parser
import Helper
import Numeric.Natural

maxCubeCounts :: CubeCounts
maxCubeCounts =
  CubeCounts
    { red = 12,
      green = 13,
      blue = 14
    }

isPullPossible :: CubeCounts -> Bool
isPullPossible pull =
  all isColourPossible [red, green, blue]
  where
    isColourPossible :: (CubeCounts -> Count) -> Bool
    isColourPossible colour = colour pull <= colour maxCubeCounts

isGamePossible :: Game -> Bool
isGamePossible Game {pulls} =
  all isPullPossible pulls

sumPossibleGameIds :: [Game] -> Natural
sumPossibleGameIds games =
  let possibleGames = filter isGamePossible games
      gameIds = gameId <$> possibleGames
   in sum gameIds

main :: IO ()
main = parse'n'process parseGames sumPossibleGameIds
