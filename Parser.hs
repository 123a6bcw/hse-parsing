module Parser (parse) where -- only expose the top-level parsing function

import Combinators
import qualified Tokenizer as T
import Prelude hiding (lookup, (>>=), map, pred, return, elem)

data AST = ASum T.Operator AST AST
         | AProd T.Operator AST AST
         | AAssign String AST
         | ANum Integer
         | AIdent String
         | AUnaryMinus T.Operator AST
         | APow T.Operator AST AST
         | ABreak AST AST -- ;
         | AConcat AST AST -- ++
         | AList AST -- [AST]
         | AEmptyList -- []
         | AComma AST AST -- a, b (inside [])

-- TODO: Rewrite this without using Success and Error
-- NOT DONE! :(
parse :: String -> Maybe (Result AST)
parse input =
  case input of
    [] -> Nothing
    _ -> case expressionWithBreak input of
           Success (tree, ts') ->
             if null ts'
             then Just (Success tree)
             else Just (Error ("Syntax error on: " ++ show ts')) -- Only a prefix of the input is parsed
           Error err -> Just (Error err) -- Legitimate syntax error

{-
I realised ; as an operator with the lowest priority. 
expression with break - either severalLists or several severalLists separated by operator ';'
-}
expressionWithBreak :: Parser AST
expressionWithBreak =
    (
        (severalListsOrSingleExpression) >>= \l ->
        breakOp >>= \op ->
        (expressionWithBreak) >>= \r -> return (ABreak l r)
    ) 
    <|> severalListsOrSingleExpression

{-
++ - operator with the higher proirity
severalLists --- either a line of assigments to severalLists, concat of several listAround, just listAround or just an expression
-}

severalListsOrSingleExpression :: Parser AST
severalListsOrSingleExpression = 
  (expression True)
  <|> severalLists
  

severalLists :: Parser AST
severalLists =
  ( identifier >>= \(AIdent i) ->
      assignment |>
        severalLists >>= \e -> return (AAssign i e)
  ) <|>
  ( listAround >>= \l ->
      concatOp >>= \c ->
         severalLists >>= \r -> return (AConcat l r)
  )
  <|> (listAround >>= \t -> return t )

{-
Either Empty List (special case in my realisation), identifier or listExpression inside square brackets (that is impotant, because it's the thing that guarantee we won't go into 
                                                                                                         endless recursion)
-}

listAround :: Parser AST
listAround =
  (lsparen >>= \l1 ->
      rsparen >>= \r1 -> return AEmptyList
  )
  <|> (identifier >>= \(AIdent i) -> return (AIdent i))
  <|>
  ( lsparen >>= \l1 ->
      listExpression >>= \t ->
        rsparen >>= \r1 -> return (AList t)
  )

{-
Either severalLists (no endless recursion since we chopped square brackets) or severalLists connected with Comma. That alows us expressions like [[a]++[b], c]
-}

listExpression :: Parser AST
listExpression = 
    ( severalListsOrSingleExpression >>= \l ->
         comma >>= \c ->
           listExpression >>= \r -> return (AComma l r)
    ) <|> (severalListsOrSingleExpression)
    
{-
  At some point, when we will run out of squareBrackest, severalLists will be just (Expression True), so here goes expression
-}
  
{-
Unary minus can only be applied to the first factor (!) in the rightest part of assigment or in parenthed expression
I choosed that way because -(5^2) != (-5) ^ 2, I decided that -5^2 = (-5) ^ 2 != -(5^2) 

Mostly the same as my previous homework. Bool in expression means expression can start with unary minus (except for the left part of the assigment), same for highTerm, Term and Factor
highTerm is terms connected with */, term --- factors connected with ^
I choosed to use Bool variable so we will no allow expressions like 5 + -2, we allow unary minus only in the begining of the initial expression, in the rightmost part of assigments or
in the begining of parenthed expression
-}

expression :: Bool ->  Parser AST
expression t =
    ( identifier >>= \(AIdent i) ->
    assignment |>
    (expression t) >>= \e -> return (AAssign i e)
    )
    <|>
    (
    (
    ( identifier >>= \(AIdent i) ->
        assignment >>= \e -> return (AEmptyList)
    )
    <|>
    ( identifier >>= \(AIdent i) ->
        concatOp >>= \e -> return (AEmptyList)
    ) 
    <|>
    ( identifier >>= \(AIdent i) ->
        breakOp >>= \e -> return (AEmptyList)
    )    
    ) 
    <!|> {- If after identifier there is an assigment to something that is not expression, ++ or ; then this identifier is not expression identifier, but list identifier
            therefore we should return Error. The left part of operator <!|> does that chechking. If it return Success, <!|> return Error (due to definition of <!|>), otherwise this is
            expression identifier and it return the second part.
          -}
    (
    ( (highTerm t)       >>= \l  -> -- Here the identifier is parsed twice :(. Whatever does it mean.
            plusMinus  >>= \op ->
            (expression False) >>= \r  -> return (ASum op l r)
    )
    <|> highTerm t
    )
    )

highTerm :: Bool -> Parser AST
highTerm t =
   (term t) >>= \l ->
  ( ( divMult >>= \op ->
      (highTerm False)    >>= \r  -> return (AProd op l r)
    )
    <|> return l
  )

term :: Bool -> Parser AST
term t =
    (factor t) >>= \l ->
      ( ( pw >>= \op ->
          (term False)    >>= \r  -> return (APow op l r)
        )
        <|> return l
      )

factor :: Bool -> Parser AST
factor t = case t of
  True -> ( unaryMinus >>= \op ->
                (factor False) >>= \l -> return (AUnaryMinus op l)
            ) <|> factor False
  False -> 
      ( lparen |>
        (expression True) >>= \e ->
        rparen |> return e -- No need to keep the parentheses
      )
      <|> identifier
      <|> number

number :: Parser AST
number      = map (ANum) (findNumber)

identifier :: Parser AST
identifier = map (AIdent) (findName)

--I had to change Char to String so we will support "++" as an operator without much pain

lparen :: Parser String
lparen = char '('

rparen :: Parser String
rparen = char ')'

lsparen :: Parser String
lsparen = char '['

rsparen :: Parser String
rsparen = char ']'

assignment :: Parser String
assignment = char '='

plusMinus :: Parser T.Operator
plusMinus = map T.operator (char '+' <|> char '-')

divMult :: Parser T.Operator
divMult   = map T.operator (char '/' <|> char '*')

pw :: Parser T.Operator
pw   = map T.operator (char '^')

unaryMinus :: Parser T.Operator
unaryMinus   = map T.operator (char '-')

breakOp :: Parser T.Operator
breakOp   = map T.operator (char ';')

comma :: Parser T.Operator
comma   = map T.operator (char ',')

concatOp :: Parser T.Operator
concatOp   = map T.operator (twochars "++")


--make it as a separated function so we could normally write [ ++ AST ++ ] and a ++ ',' ++ b
coolTree :: Int -> (String -> String)
coolTree n = if n > 0 then \s -> concat (replicate (n - 1) "| ") ++ "|_" ++ s else id

instance Show AST where
  show tree = "\n" ++ show' 0 tree
    where
      show' n t =
        case t of 
            --So there will be no meaningless string before the first element in list
            AComma l r -> show' (n) l ++ "\n" ++ (coolTree n ",") ++ "\n" ++ show' (n) r
            _ ->
                (coolTree n)
                (case t of
                          ASum  op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                          AProd op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                          AAssign  v e -> (show v) ++ " =\n" ++ show' (ident n) e
                          ANum   i     -> show i
                          AIdent i     -> show i
                          APow op l r  -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                          AUnaryMinus op t -> showOp op : "\n" ++ show' (ident n) t
                          ABreak l r -> (show' 0 l) ++ "\n\n" ++ (show' 0 r)
                          AConcat l r -> "++" ++ "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                          AEmptyList -> "[" ++ "\n" ++ (coolTree n "]") 
                          AList l -> "[" ++ "\n" ++ show' (ident n) l ++ "\n" ++ (coolTree n "]")
                          )
      ident = (+1)
      showOp T.Plus  = '+'
      showOp T.Minus = '-'
      showOp T.Mult  = '*'
      showOp T.Pow   = '^'
      showOp T.Div   = '/'
