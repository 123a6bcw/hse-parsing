module Combinators where
-- Make sure that the names don't clash
import Prelude hiding (lookup, (>>=), map, pred, return, elem)
import Tokenizer

-- Input abstraction
type Input = String

-- Result is polymorphic in the ... result
data Result r = Success r
              | Error String
              deriving (Show)

-- The result of parsing is some payload r and the suffix which wasn't parsed
type Parser r = Input -> Result (r, Input)

infixl 6 <|>
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = \inp ->
  case p inp of
    Error _ -> q inp
    result  -> result

infixl 5 <!|>
(<!|>) :: Parser a -> Parser a -> Parser a
p <!|> q = \inp ->
  case p inp of
    Error _ -> q inp
    result  -> Error "h"

deleteWhiteSpaces :: String -> String
deleteWhiteSpaces "" = ""
deleteWhiteSpaces xl@(x : xs) | isWhiteSpace x = deleteWhiteSpaces xs
                              | otherwise = xl 

-- Sequential combinator: if the first parser successfully parses some prefix, the second is run on the suffix
-- The second parser is supposed to use the result of the first parser
infixl 7 >>=
(>>=) :: Parser a -> (a -> Parser b ) -> Parser b
p >>= q = \inp ->
  case p (deleteWhiteSpaces inp) of
    Success (r, inp') -> q r (deleteWhiteSpaces inp')
    Error err -> Error err

-- Sequential combinator which ignores the result of the first parser
infixl 7 |>
(|>) :: Parser a -> Parser b -> Parser b
p |> q = p >>= const q

-- Succeedes without consuming any input, returning a value
return :: a -> Parser a
return r inp = Success (r, inp)

-- Always fails
zero :: String -> Parser a
zero err = const $ Error err

-- Chops of the first element of the string
elem :: Parser String
elem (c : cs) = Success ([c], cs)
elem [] = Error "Empty string"

-- chop the maximum prefix of the string which is number
chopInt :: Integer -> String -> (Integer, String)
chopInt a []  = (a, [])
chopInt a xl@(x : xs) | isDigit x = chopInt (a * 10 + (digit x)) xs 
                      | otherwise = (a, xl)

--chop the maximum prefix of the string which is an identificator
chopName :: String -> (String, String)
chopName [] = ("", "")
chopName xl@(x : xs) | isAlpha x = let (name, tail) = chopName xs 
                                  in (x : name, tail)
                     | otherwise = ([], xl)

-- chops an integer from the begining and creates corresponding Parser 
findNumber :: Parser Integer
findNumber [] = Error "Empty string is not a number"
findNumber s@(x : xs) | not $ isDigit x = Error ("First letter isn't a digit " ++ s)
findNumber s = Success $ chopInt 0 s

--same for identificator
findName :: Parser String
findName [] = Error "Empty string is not an indent"
findName (x : xs) | not $ isAlpha x = Error "This isn't an indent"
findName s = Success $ chopName s

-- Checks if the first character of the string is the given one
char :: Char -> Parser String
char c = sat (== [c]) elem

-- Chops two of the first elements of the string so we can parse "++" as an operator
elem' :: Parser String
elem' [] = Error "Empty string"
elem' (c : []) = Error "Not enough symbols"
elem' (c1 : c2 : cs) = Success (c1 : c2 : [], cs)

-- Same purpose 
twochars :: String -> Parser String
twochars c = sat (== c) elem'

-- Checks if the parser result satisfies the predicate
sat :: (a -> Bool) -> Parser a -> Parser a
sat pred parser inp =
  case parser inp of
    Success (r, inp') | pred r ->  Success (r, inp')
    Success _ -> Error ("Predicate is not satisfied" ++ inp)
    Error err -> Error err

-- Applies the function to the result of the parser
map :: (a -> b) -> Parser a -> Parser b
map f parser inp =
  case parser inp of
    Success (r, inp') -> Success (f r, inp')
    Error err -> Error err
