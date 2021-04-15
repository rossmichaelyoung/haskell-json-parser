module Main where

import Control.Applicative
import Control.Monad
import Data.Char

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Int
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(JsonValue, JsonValue)] -- JsonObject [(JsonString, JsonValue)]
  deriving (Show, Eq)

newtype Parser a = Parser {runParser :: String -> Either String (String, a)}

instance Functor Parser where
  fmap f a = Parser $ runParser a >=> \(input', x) -> Right (input', f x)

instance Applicative Parser where
  pure = return
  (Parser a) <*> (Parser b) = Parser $ a >=> (\(input', f) -> b input' >>= \(input'', x) -> Right (input'', f x))

instance Monad Parser where
  return x = Parser $ \input -> Right (input, x)
  p >>= f = join $ fmap f p

instance Semigroup (Parser a) where
  (Parser p1) <> (Parser p2) = Parser $ \input -> p1 input <> p2 input

instance Alternative Parser where
  empty = Parser $ \input -> Left "empty"
  a <|> b = a <> b

charParser :: Char -> Parser Char
charParser x = Parser f
  where
    f [] = Left "Empty Input"
    f (y : ys) = if y == x then Right (ys, x) else Left $ "Could not parse " ++ "\'" ++ [y] ++ "\'"

stringParser :: String -> Parser String
stringParser = mapM charParser

spanParser :: (Char -> Bool) -> Parser String
spanParser f = Parser $ \input -> let (token, rest) = span f input in Right (rest, token)

stringLiteralParser :: Parser String
stringLiteralParser = charParser '"' *> spanParser (/= '"') <* charParser '"'

wsParser :: Parser String
wsParser = spanParser isSpace

sepByParser :: Parser a -> Parser b -> Parser [b]
sepByParser a b = (:) <$> b <*> many (a *> b) <|> pure []

jsonNull :: Parser JsonValue
jsonNull = fmap (const JsonNull) (stringParser "null")

jsonBool :: Parser JsonValue
jsonBool =
  fmap (\_ -> JsonBool True) (stringParser "true")
    <> fmap (\_ -> JsonBool False) (stringParser "false")

jsonNumber :: Parser JsonValue
jsonNumber =
  JsonNumber . read
    <$> Parser
      ( runParser (spanParser isDigit)
          >=> ( \(rest, token) ->
                  if null token
                    then Left "Could not parse"
                    else Right (rest, token)
              )
      )

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteralParser

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charParser '[' *> wsParser *> arrayElements <* wsParser <* charParser ']')
  where
    arrayElements = sepByParser (wsParser *> charParser ',' <* wsParser) jsonParser

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charParser '{' *> wsParser *> keyValuePairList <* wsParser <* charParser '}')
  where
    keyValuePair = fmap (,) jsonString <*> (wsParser *> charParser ':' *> wsParser *> jsonParser)
    keyValuePairList = sepByParser (wsParser *> charParser ',' <* wsParser) keyValuePair

jsonParser :: Parser JsonValue
jsonParser = jsonNull <> jsonBool <> jsonNumber <> jsonString <> jsonArray <> jsonObject

getJsonData :: FilePath -> Parser a -> IO (Either String a)
getJsonData fileName parser = readFile fileName >>= \fileContent -> return (snd <$> runParser parser fileContent)

main :: IO ()
main =
  putStrLn "Enter JSON File Name"
    >> getLine
    >>= \fileName ->
      getJsonData fileName jsonParser
        >>= \(Right jsonData) -> print jsonData