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

data PulledCubes = PulledCubes
  { red :: Integer
  , green :: Integer
  , blue :: Integer
  }
  deriving Show

data Game = Game
  { gameId :: Integer
  , pulls :: [PulledCubes]
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

parsePull :: Parser PulledCubes
parsePull = do
  colourCounts <- parseColourCount `sepBy` string' ", "
  let f = fromMaybe 0 . flip lookup colourCounts
      [red, green, blue] = f <$> [Red, Green, Blue]
  pure $ PulledCubes {red, green, blue}

parseGame :: Parser Game
parseGame = do
  string' "Game "
  gameId <- integer
  string' ": "
  pulls <- parsePull `sepBy` string' "; "
  pure $ Game {gameId, pulls}

parser :: Parser [Game]
parser = parseGame `sepEndBy` newline

maxCubes :: PulledCubes
maxCubes =
  PulledCubes
    { red = 12
    , green = 13
    , blue = 14
    }

isPullPossible :: PulledCubes -> Bool
isPullPossible pull =
  all isColourPossible [red, green, blue]
    where
      isColourPossible :: (PulledCubes -> Integer) -> Bool
      isColourPossible colour = colour pull <= colour maxCubes

isGamePossible :: Game -> Bool
isGamePossible game =
  all isPullPossible (pulls game)

run :: String -> Integer
run input =
  let games =
        case parse parser "huh" input of
          Left err -> error $ show err
          Right res -> res
      possibleGames = filter isGamePossible games
      gameIds = gameId <$> possibleGames
   in sum gameIds

wrap :: Show a => (String -> a) -> (String -> String)
wrap f input
  = show (f input) <> "\n"

main :: IO ()
main = interact (wrap run)
