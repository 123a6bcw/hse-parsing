module Parser where

import Tokenizer
import Prelude hiding (lookup)

data AST = ASum Operator AST AST
         | AProd Operator AST AST
         | APow Operator AST AST
         | AAssign String AST
         | ANum Integer
         | AIdent String
         | AunaryMinus Operator AST

parse :: String -> Maybe AST
parse input =
  let ts = tokenize input in
  case ts of
    [TEof] -> Nothing
    _ -> let (tree, ts') = expression True ts in
         if ts' == [TEof]
         then Just tree
         else error ("Parsing error on: " ++ show ts')

{-
  Expression по прежнему обозначает любое выражение
  High Term - выражение, содержащее в себе только умножение, деление и возведение в степень. Term - только возведение в степень. Тем самым возведение в степень имеет больший приоритет.
  Переменная типа Bool в expression отвечает за то, может ли первый High Term выражения содержать перед собой унарный минус. В первый Expression в программе передаётся True.
  Эта переменная передаётся в первый High Term, в остальные передаётся False. Переменная булева типа для Хай терма означает, может ли данный хай терм иметь перед собой унарный минус
  Внутри High Term передаётся True только в parenthesised expression
  Таким образом, возможно две ситуации: старший терм начинается с унарного минуса, т.к. является первым термом после скобок или первым термом начального выражения, либо может быть выражение вида
  (-(expression))
  Я сделал так, так как это наиболее естественное для меня поведение унарного минуса
-}

expression :: Bool -> [Token] -> (AST, [Token])
expression can ts =
  let (highTermNode, ts') = highTerm can ts in
  case lookup ts' of
    TOp op | op == Plus || op == Minus ->
      let (exprNode, ts'') = expression False (accept ts') in
      (ASum op highTermNode exprNode, ts'')
    TAssign ->
      case highTermNode of
        AIdent v -> let (exprNode, ts'') = expression True (accept ts') in
                    (AAssign v exprNode, ts'')
        _ -> error "Syntax error: assignment is only possible to identifiers"
    _ -> (highTermNode, ts')

highTerm :: Bool -> [Token] -> (AST, [Token])
highTerm can ts =
 case lookup ts of
       TOp op | op == Minus ->
            if can
            then let (highTermNode, ts') = highTerm False (accept ts) in 
                (AunaryMinus Minus highTermNode, ts')
            else error "Syntax error: wrong usage of unary minus"
       _ -> 
            let (termNode, ts') = term ts in
                case lookup ts' of
                    TOp op | op == Mult || op == Div ->
                        let (highTermNode, ts'') = highTerm False (accept ts') in
                        (AProd op termNode highTermNode, ts'')
                    _ -> (termNode, ts')

term :: [Token] -> (AST, [Token])
term ts =
         let (factNode, ts') = factor ts in
          case lookup ts' of
            TOp op | op == Pow ->
              let (termNode, ts'') = term (accept ts') in
              (APow op factNode termNode, ts'')
            _ -> (factNode, ts')

factor :: [Token] -> (AST, [Token])
factor ts =
  case lookup ts of
    TLParen ->
      let (exprNode, ts') = expression True (accept ts) in
      case lookup ts' of
        TRParen -> (exprNode, accept ts')
        _ -> error "Syntax error: mismatched parentheses"
    TIdent v -> (AIdent v, accept ts)
    TNumber d -> (ANum d, accept ts)
    _ -> error "Syntax error: factor can only be a number, an identifier or a parenthesised expression"

lookup :: [Token] -> Token
lookup = head

accept :: [Token] -> [Token]
accept = tail

instance Show AST where
  show tree = "\n" ++ show' 0 tree
    where
      show' n t =
        (if n > 0 then \s -> concat (replicate (n - 1) "| ") ++ "|_" ++ s else id)
        (case t of
                  ASum  op l r     -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AProd op l r     -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  APow op l r     -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AAssign  v e     -> show v ++ " =\n" ++ show' (ident n) e
                  ANum   i         -> show i
                  AIdent i         -> show i
                  AunaryMinus op t -> showOp op : "\n" ++ show' (ident n) t)
      ident = (+1)
      showOp Plus  = '+'
      showOp Minus = '-'
      showOp Mult  = '*'
      showOp Pow   = '^'
      showOp Div   = '/'
