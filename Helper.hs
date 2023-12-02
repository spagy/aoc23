module Helper
  ( parse'n'process,
  )
where

import Text.Parsec
import Text.Parsec.String

parseOrCrash :: Parser a -> String -> a
parseOrCrash parser input =
  case parse parser "" input of
    Left err -> error (show err)
    Right res -> res

parse'n'process :: (Show b) => Parser a -> (a -> b) -> IO ()
parse'n'process parser process = do
  rawInput <- getContents
  let parsedInput = parseOrCrash parser rawInput
  print $ process parsedInput
