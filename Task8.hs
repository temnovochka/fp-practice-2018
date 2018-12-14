module Parsers where

import Text.Parsec hiding(digit)

type Parser a = Parsec String () a

digit :: Parser Char
digit = oneOf $ ['0'..'9']

number :: Parser Integer
number = read <$> many1 digit

byNumber :: Char 
                    -> (Integer -> Integer -> Integer) 
                    -> Parser Integer
                    -> Parser (Integer -> Integer)
byNumber symbol func base =
                    do
                        char symbol
                        spaces
                        n <- base
                        spaces
                        return $ (`func` n)

multNumber :: Parser (Integer -> Integer)
multNumber = byNumber '*' (*) expr

divNumber :: Parser (Integer -> Integer)
divNumber = byNumber '/' div expr

multiplication :: Parser Integer
multiplication = do
                    x <- expr
                    spaces
                    ys <- many (multNumber <|> divNumber)
                    return $ foldl (\ x f -> f x) x ys

plusNumber :: Parser (Integer -> Integer)
plusNumber = byNumber '+' (+) multiplication

minusNumber :: Parser (Integer -> Integer)
minusNumber = byNumber '-' (-) multiplication

addition :: Parser Integer
addition = do
                x <- multiplication
                spaces
                ys <- many (plusNumber <|> minusNumber)
                return $ foldl (\ x f -> f x) x ys

expr :: Parser Integer
expr = number 
        <|> do 
                char '('
                spaces
                res <- addition
                char ')'
                spaces
                return $ res

root :: Parser Integer
root = do
            spaces
            p <- addition
            eof
            return $ p

main =
        do
            s <- getLine
            putStrLn $ show $ parse root "<input>" s
main