-- https://hasura.io/blog/parser-combinators-walkthrough
{-# LANGUAGE LambdaCase #-}

data ParseError = ParseError String String

newtype Parser a = Parser {
    runParser :: String -> (String, Either ParseError a)
}

-- basic parsers

any :: Parser Char
any = Parser $ \case
    -- Some input left, we parse the first character
    (x:xr) -> (xr, Right x)
    -- No input left, parser will fail
    [] -> ("", Left $ ParseError
        "any character" -- expected
        "the end of the input" -- encountered
        )

eof :: Parser ()
eof = Parser $ \case
    -- No input left, parser succeeds
    [] -> ([], Right ())
    input@(c:_) -> (input, Left $ ParseError
        "the end of the input" -- expected
        [c] -- encountered    
        )