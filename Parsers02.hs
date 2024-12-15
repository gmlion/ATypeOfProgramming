module P2 where

data Parser a = Parser { runParser :: a -> (a, Maybe b) }

boolFromChar :: Char -> Either String Bool
boolFromChar =
    \case ->
        't' -> Right True
        'f' -> Right False
        _ -> Left "Expected 't' of 'f'"

