module Task8 where

import Text.Parsec hiding(digit)

type Parser a = Parsec String () a

digit :: Parser Char
digit = oneOf $ ['0'..'9'] 

intNum :: Parser String
intNum = many1 digit

floatPart :: Parser String
floatPart = do
                char '.'
                res <- many1 digit
                return $ ('.':res)

number :: Parser Float
number = do
            num <- intNum
            opt <- option "" floatPart
            return $ read $ (num ++ opt)

byNumber :: Char 
                    -> (Float -> Float -> Float) 
                    -> Parser Float
                    -> Parser (Float -> Float)
byNumber symbol func base =
                    do
                        char symbol
                        spaces
                        n <- base
                        spaces
                        return $ (`func` n)

unMinusNumber :: Parser Float
unMinusNumber = do
                    res <- byNumber '-' (-) expr
                    return $ res 0

unMinus :: Parser Float
unMinus = do
            spaces
            ys <- (unMinusNumber <|> expr)
            return $ ys

powNumber :: Parser (Float -> Float)
powNumber = byNumber '^' (**) unMinus

power :: Parser Float
power = do
            x <- unMinus
            spaces
            ys <- many (powNumber)
            return $ foldl (\ x f -> f x) x ys

multNumber :: Parser (Float -> Float)
multNumber = byNumber '*' (*) power

divNumber :: Parser (Float -> Float)
divNumber = byNumber '/' (/) power

multiplication :: Parser Float
multiplication = do
                    x <- power
                    spaces
                    ys <- many (multNumber <|> divNumber)
                    return $ foldl (\ x f -> f x) x ys

plusNumber :: Parser (Float -> Float)
plusNumber = byNumber '+' (+) multiplication

minusNumber :: Parser (Float -> Float)
minusNumber = byNumber '-' (-) multiplication

addition :: Parser Float
addition = do
                x <- multiplication
                spaces
                ys <- many (plusNumber <|> minusNumber)
                return $ foldl (\ x f -> f x) x ys

expr :: Parser Float
expr = number 
        <|> do 
                char '('
                spaces
                res <- addition
                char ')'
                spaces
                return $ res

root :: Parser Float
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