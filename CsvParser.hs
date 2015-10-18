module CsvParser (
  Csv
, parse
) where

import Data.Char
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec hiding (parse)

type Csv = [[(String, String)]]

lexeme :: Parser a -> Parser a
lexeme parser = parser <* (many $ char ' ')

symbol :: String -> Parser String
symbol = lexeme . string

sepByCommas :: Parser a -> Parser [a]
sepByCommas = (flip sepBy) (symbol ",")

identifier :: Parser String
identifier = lexeme $ do
  word <- many $ satisfy isAlphaNum
  return $ word

csvHeaders :: Parser [String]
csvHeaders = sepByCommas identifier

csvLine :: Parser [String]
csvLine = sepByCommas identifier <* char '\n'

csvParser :: Parser Csv
csvParser = do
    hs <- csvHeaders
    char '\n'
    ls <- many1 csvLine
    return $ zipHeadersAndValues hs ls

parse :: String -> Either ParseError Csv
parse str = P.parse csvParser "" str

zipHeadersAndValues :: [String] -> [[String]] -> Csv
zipHeadersAndValues hs vs = map (zip hs) vs
