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
    deriving (Show)

parseDigit :: Parser String Digit
parseDigit =
    Parser (
        \c ->
            case c of
            n:xs | n == '0' -> (xs, Just Zero)
                 | n == '1' -> (xs, Just One)
                 | n == '2' -> (xs, Just Two)
                 | n == '3' -> (xs, Just Three)
                 | n == '4' -> (xs, Just Four)
                 | n == '5' -> (xs, Just Five)
                 | n == '6' -> (xs, Just Six)
                 | n == '7' -> (xs, Just Seven)
                 | n == '8' -> (xs, Just Eight)
                 | n == '9' -> (xs, Just Nine)
            nomatch -> (nomatch, Nothing)
    )

parseMany :: Parser String b -> Parser String [b]
parseMany parser =
    Parser (
        \s ->
            let parseManyInner res s = case runParser parser s of
                    ([], Just p) -> ([], Just p : res)
                    ([], Nothing) -> ([], res)
                    (rest, Nothing) -> (rest, res)
                    (rest, Just p) -> parseManyInner (Just p : res) rest
                (rest, parsed) = parseManyInner [] s
                in
                    (rest, sequenceA . reverse $ parsed)
    )
    

