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
  putStrLn "\n\nОтсюда идут изменения деревьев на правильную ассоциативность\n\n"
  runParserWithRightAssociativity "a - b - c"
  runParserWithRightAssociativity "a - (b - c)"
  runParserWithRightAssociativity "1 * 2 * 3 * (4 / 5 / 6 / (7  / 8))"
