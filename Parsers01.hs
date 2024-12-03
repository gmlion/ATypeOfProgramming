-- First parser from string to natural number

-- naturalFromString :: String -> Maybe Natural

-- We will need some helpers, like

isDigit :: Char -> Bool

isDigit '0' = True
isDigit '1' = True
isDigit '2' = True
isDigit '3' = True
isDigit '4' = True
isDigit '5' = True
isDigit '6' = True
isDigit '7' = True
isDigit '8' = True
isDigit '9' = True
isDigit _ = False

data Parser a b = Parser { runParser :: a -> (a, Maybe b) }

data Digit =
    Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine

parseDigit :: Parser String Digit
parseDigit =
    Parser (
        \c ->
            case c of
            n:xs | n == '0' -> (xs, Just Zero)
    )

parseMany :: Parser -> Parser
parseMany parser =
    Parser (
        \s ->
            
    )
    

