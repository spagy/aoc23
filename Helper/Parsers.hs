module Helper.Parsers where

import Numeric.Natural
import Text.Parsec
import Text.Parsec.String

natural :: Parser Natural
natural = read <$> many1 digit
