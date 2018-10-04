module Tokenizer where

data Token = TNumber Integer
           | TIdent String
           | TOp Operator
           | TLParen
           | TRParen
           | TAssign
           | TEof
           deriving (Show, Eq)

data Operator = Plus
              | Minus
              | Mult
              | Div
              | Pow
              deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = [TEof]
tokenize cl@(c : cs) | isOperator c   = TOp (operator c) : tokenize cs
                    | isDigit c      = let (num, tail) = getNumber cl
                                       in TNumber num : tokenize tail  
                    | isAlpha c      = let (name, tail) = getName cl
                                       in TIdent name : tokenize tail
                    | c == '('       = TLParen : tokenize cs
                    | c == ')'       = TRParen : tokenize cs
                    | c == '='       = TAssign : tokenize cs
                    | isWhiteSpace c = tokenize cs
                    | otherwise = error ("Lexical error: unacceptable character " ++ [c])

isOperator :: Char -> Bool
isOperator x = x `elem` "+-*/^"

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Mult
           | c == '/' = Div
           | c == '^' = Pow
operator c = error ("Lexical error: " ++ c : " is not an operator!")

isDigit :: Char -> Bool
isDigit x = x `elem` "0123456789"

digit :: Char -> Integer
digit c | c == '0' = 0
        | c == '1' = 1
        | c == '2' = 2
        | c == '3' = 3
        | c == '4' = 4
        | c == '5' = 5
        | c == '6' = 6
        | c == '7' = 7
        | c == '8' = 8
        | c == '9' = 9
digit c = error ("Lexical error: " ++ c : " is not a digit!")

isAlpha :: Char -> Bool
isAlpha c = c `elem` ['a' .. 'z']

alpha :: Char -> Char
alpha c = c

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\n"

getNumber :: String -> (Integer, String)
--Хочу сразу строить Integer, а не строить промежуточную строку, которую потом нужно переводить в число. Для этого приходится пользоваться аккамулятором
getNumber = accumulateNumber 0 where
                accumulateNumber :: Integer -> String -> (Integer, String)
                accumulateNumber s [] = (s, [])  --Если число находится в конце строки. 
                                                 --При этом не будет проблемы, что пустая строка будет восприниматься как число, т.к. мы вызываем эту функцию только при обнаружении цифры 
                accumulateNumber s xl@(x : xs) | isDigit x = accumulateNumber (s * 10 + (digit x)) xs
                                               | otherwise = (s, xl)
                        
getName ::String -> (String, String)
getName [] = ("", "")
--Мотивация та же
getName xl@(x : xs) | isAlpha x = let (name, tail) = getName xs 
                                  in (x : name, tail)
                    | otherwise = ([], xl)
