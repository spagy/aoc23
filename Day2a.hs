#!/usr/bin/env runghc

import Help.Parsers

import Data.Functor
import Data.Maybe
import Text.Parsec
import Text.Parsec.String
import Numeric.Natural

{--- TYPES ---}

data Colour
  = Red
  | Green
  | Blue
  deriving Eq

type Count = Natural

data CubeCounts = CubeCounts
  { red :: Count
  , green :: Count
  , blue :: Count
  }

data Game = Game
  { gameId :: Natural
  , pulls :: [CubeCounts]
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
      { red = countOf Red
      , green = countOf Green
      , blue = countOf Blue
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

{--- PROCESSING ---}
maxCubeCounts :: CubeCounts
maxCubeCounts =
  CubeCounts
    { red = 12
    , green = 13
    , blue = 14
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

process :: [Game] -> Natural
process games =
  let possibleGames = filter isGamePossible games
      gameIds = gameId <$> possibleGames
   in sum gameIds

{--- FAFF ---}
parseOrCrash :: String -> [Game]
parseOrCrash input =
  case parse parseGames "" input of
    Left err -> error (show err)
    Right res -> res

wrap :: Show a => (String -> a) -> (String -> String)
wrap f input
  = show (f input) <> "\n"

main :: IO ()
main = interact $ wrap (process . parseOrCrash)
