module Main where

import qualified Control.Exception as E
import Data.Maybe
import Parser
import Reverser

runParser :: String -> IO ()
runParser input = do
  putStrLn input
  print $ parse input
  putStrLn ""

tryToParse :: String -> IO ()
tryToParse s = E.catch (runParser s) handler where
                     handler :: E.SomeException -> IO ()
                     handler ex = putStrLn $ "Caught exception: " ++ show ex

runParserWithRightAssociativity :: String -> IO ()
runParserWithRightAssociativity input = do
  putStrLn input
  print $ changeAssociativity (fromJust (parse input))
  putStrLn ""


--мой набор тестов
main :: IO ()
main = do
  runParser " 1 - 2 - 3 "
  runParser " (((9)))"
  runParser " 1 * 2 - 3 / 4 + 5"
  runParser " -var"
  runParser "-12 * (-3 + 5) + var"
  runParser "-1 ^ (-22 + 13) ^ (-var)"
  runParser "2 * 5 ^ 9 ^ (-3) * 3 * 9 / 7 ^ 1 ^ 1 ^ var * 1"
  runParser "1 ^ (-(-5 * 2))"
  runParser "-2 * 5 ^ var ^ 3 + 3^1^k^1 - 1*t/1"
  runParser "-x^y"
  putStrLn "\n\nОтсюда идут выражения, в которых должны быть ошибки\n\n"
  tryToParse "--var"
  tryToParse "-3 - - 9"
  tryToParse "-var = 1"
  tryToParse "2^-1"
  tryToParse "2^1 - -2*5 + 1"
  putStrLn "\n\nОтсюда идут изменения деревьев на правильную ассоциативность\n\n"
  runParserWithRightAssociativity "1+2-3-4+5"
  runParserWithRightAssociativity "a * b / c * d"
