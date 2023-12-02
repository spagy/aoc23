module Day2.Parser where

import Data.Functor
import Data.Maybe
import Helper.Parsers
import Numeric.Natural
import Text.Parsec
import Text.Parsec.String

{--- TYPES ---}
data Colour
  = Red
  | Green
  | Blue
  deriving (Eq)

type Count = Natural

data CubeCounts = CubeCounts
  { red :: Count,
    green :: Count,
    blue :: Count
  }

data Game = Game
  { gameId :: Natural,
    pulls :: [CubeCounts]
  }

{--- PARSERS ---}
parseColourCount :: Parser (Colour, Count)
parseColourCount = do
  let red = string' "red" $> Red
      green = string' "green" $> Green
      blue = string' "blue" $> Blue

  count <- natural
  spaces
  colour <- red <|> green <|> blue
  pure (colour, count)

parsePull :: Parser CubeCounts
parsePull = do
  colourCounts <- parseColourCount `sepBy` string' ", "
  let countOf colour = fromMaybe 0 $ lookup colour colourCounts
  pure $
    CubeCounts
      { red = countOf Red,
        green = countOf Green,
        blue = countOf Blue
      }

parseGame :: Parser Game
parseGame = do
  string' "Game "
  gameId <- natural
  string' ": "
  pulls <- parsePull `sepBy` string' "; "
  pure $ Game {gameId, pulls}

parseGames :: Parser [Game]
parseGames = parseGame `sepEndBy` newline
