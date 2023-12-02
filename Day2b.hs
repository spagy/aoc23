#!/usr/bin/env runghc

import Day2.Parser
import Helper
import Helper.Parsers
import Numeric.Natural

minPossibleCubesInGame :: Game -> CubeCounts
minPossibleCubesInGame Game {pulls} =
  let maxCountOf getColour = maximum $ getColour <$> pulls
   in CubeCounts
        { red = maxCountOf red,
          green = maxCountOf green,
          blue = maxCountOf blue
        }

cubeCountsToPower :: CubeCounts -> Natural
cubeCountsToPower CubeCounts {red, green, blue} =
  red * green * blue

sumGamePowers :: [Game] -> Natural
sumGamePowers games =
  let cubeCounts = minPossibleCubesInGame <$> games
      powers = cubeCountsToPower <$> cubeCounts
   in sum powers

main :: IO ()
main = parse'n'process parseGames sumGamePowers
