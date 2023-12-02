#!/usr/bin/env runghc

import Debug.Trace
import Data.Functor
import Data.Maybe
import Text.Parsec
import Text.Parsec.String

dbg :: Show a => a -> a
dbg a = trace (show a) a

data Colour
  = Red
  | Green
  | Blue
  deriving Eq

data CubeCounts = CubeCounts
  { red :: Integer
  , green :: Integer
  , blue :: Integer
  }
  deriving Show

data Game = Game
  { gameId :: Integer
  , pulls :: [CubeCounts]
  }
  deriving Show

integer :: Parser Integer
integer = read <$> many1 digit

parseColourCount :: Parser (Colour, Integer)
parseColourCount = do
  let red = string' "red" $> Red
      green = string' "green" $> Green
      blue = string' "blue" $> Blue

  count <- integer
  spaces
  colour <- red <|> green <|> blue
  pure (colour, count)

parsePull :: Parser CubeCounts
parsePull = do
  colourCounts <- parseColourCount `sepBy` string' ", "
  let f = fromMaybe 0 . flip lookup colourCounts
      [red, green, blue] = f <$> [Red, Green, Blue]
  pure $ CubeCounts {red, green, blue}

parseGame :: Parser Game
parseGame = do
  string' "Game "
  gameId <- integer
  string' ": "
  pulls <- parsePull `sepBy` string' "; "
  pure $ Game {gameId, pulls}

parser :: Parser [Game]
parser = parseGame `sepEndBy` newline

minPossibleCubesInGame :: Game -> CubeCounts
minPossibleCubesInGame Game {pulls} =
  let maxCountOf getColour = maximum $ getColour <$> pulls
   in CubeCounts
     { red = maxCountOf red
     , green = maxCountOf green
     , blue = maxCountOf blue
     }

cubeCountsToPower :: CubeCounts -> Integer
cubeCountsToPower CubeCounts {red, green, blue}
  = red * green * blue

run :: String -> Integer
run input =
  let games =
        case parse parser "huh" input of
          Left err -> error $ show err
          Right res -> res
      cubeCounts = minPossibleCubesInGame <$> games
      powers = cubeCountsToPower <$> cubeCounts
   in sum powers

wrap :: Show a => (String -> a) -> (String -> String)
wrap f input
  = show (f input) <> "\n"

main :: IO ()
main = interact (wrap run)
