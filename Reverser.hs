module Reverser where

import Prelude
import Tokenizer
import Parser

{-Эта функция складывает в список ссылки на вершины AST всех факторов на текущем уровне, связанных делением или умножением, и соответствующие операторы, которыми эти термы были связаны.
Перед тем, как положить ссылку на AST в список, функция также заранее запускает функцию смену ассоциативности в каждой из этих вершин. -}

astProdToList :: AST -> ([Operator], [AST])
astProdToList  t = case t of
    AProd op l r ->
        (op : oper, (changeAssociativity l) : ast) where
          (oper, ast) = astProdToList r
    _ ->
        ([], [changeAssociativity t])


--данная функция совершает обратную операцию --- по данному списку ссылок на вершины АСТ и операций (деления или умножения), которым они были связаны, восстанавливает АСТ.
listToAstProd :: ([Operator], [AST]) -> AST 
listToAstProd ([], y:[]) = y
listToAstProd (x : xs, y : ys) = AProd x (listToAstProd (xs, ys)) y


--тоже самое, только для сложения и вычитания
astSumToList :: AST -> ([Operator], [AST])
astSumToList  t = case t of
    ASum op l r ->
        (op : oper, (changeAssociativity l) : ast) where
          (oper, ast) = astSumToList r
    _ ->
        ([], [changeAssociativity t])

listToAstSum :: ([Operator], [AST]) -> AST 
listToAstSum ([], y:[]) = y
listToAstSum (x : xs, y : ys) = ASum x (listToAstSum (xs, ys)) y

{- Если на текущем уровне нет произведения или сложения, функция просто рекурсивно запускает смену ассоциативности от сыновей
--Иначе она сначала складывает все вершины-термы соответствующей операции в список (первая функция), затем переворачивает получившиеся списки, и затем восстанавливает  дерево по таким спискам (вторая).
--Несложно понять, что именно этого мы и хотели --- перевернуть вершины так, чтобы на самом нижнем уровне складывались (перемножались) две первые вершины, на уровне повыше на них перемножалась третья вершина и т.д -}

changeAssociativity :: AST -> AST
changeAssociativity t = case t of 
                ASum op l r ->
                    listToAstProd (reverse sums, reverse nodes) where
                        (sums, nodes) = astSumToList t
                AProd op l r ->
                    listToAstProd (reverse prods, reverse nodes) where
                        (prods, nodes) = astProdToList t
                APow op l r ->
                    APow op (changeAssociativity l) (changeAssociativity r)
                AAssign s l ->
                    AAssign s (changeAssociativity l)
                AunaryMinus op l ->
                    AunaryMinus op (changeAssociativity l)
                ANum i ->
                    ANum i
                AIdent s ->
                    AIdent s
                AParen s ->
                    AParen (changeAssociativity s)
                
             
