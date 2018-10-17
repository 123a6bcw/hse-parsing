{-# LANGUAGE FlexibleInstances #-}

module Main where

import Parser
import Combinators (Result (Success, Error))

runParser :: String -> IO ()
runParser input = do
  putStrLn input
  print $ parse input
  putStrLn ""

instance {-# OVERLAPPING #-} Show a => Show (Maybe (Result a)) where
  show (Just (Success tree)) = show tree
  show (Just (Error err)) = "Syntax error: " ++ err
  show Nothing = "Empty tree"

main :: IO ()
main = do
  runParser " 1+2 "
  runParser "         ( ( (94   ) ) )      "
  runParser "1  *   24  -    3  /   4 \n \n \n + 5              "
  runParser "21   +   2            "
  runParser "           var = 13 * 42"
  runParser "      557*f^my^648*life^33"
  runParser "    -    kill   *  (-   me + (-please))   "
  runParser "-2  +5-3;     15"
  runParser "x = 13; \ny = z = 42 + 6;\n 777"
  putStrLn "here should be parse error"
  runParser "x    = -y = -13"
  
  runParser "  - 5  +    2+3"
  runParser "  - 5^(-4 + 6 * (-5) * (-2 ^ 3))  "
  runParser "a = [] ;\n [b = 13, [z], 42 + 6] ++ a ++ [31, 25];\n777"
  runParser "[a++[b], c]  "
  runParser "  [[], [], a ++ [] ++ [1,[[[]]]]]"
  runParser "2+ -22"
  runParser "2 + (-22)"
  runParser "2 ^ (-1)"
  runParser "x = y = -1"
  runParser "-5 * (-3 ^ (-2))"
  runParser " [ a + 5]  "
  runParser "  a = [] "
  runParser "a = b = [1,  2, 3 ] ++ [] ++ a"
  runParser "[ [abc = 5], [], [5, 6, [3^(-2)    ]] ]"
  putStrLn "\nдальше идут много ошибок\n"
  runParser " [1] ++ 1"
  runParser "a = [b] = c"
  runParser "[-5--5]"
  runParser "5+*2"
  runParser "gg??"
  runParser "[-1^-2]"
  runParser "@"
  runParser "[ [abc = 5], [], [5, 6, [3^-2]] ]"
