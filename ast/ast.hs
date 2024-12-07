module Ast where

-- Define the Expr data type
data Expr 
    = Plus Expr Expr 
    | Minus Expr Expr 
    | Times Expr Expr 
    | Div Expr Expr
    | Literal Float
    deriving (Show, Eq) -- Add Show and Eq for debugging and comparisons

-- Define the eval function to evaluate expressions
eval :: Expr -> Float
eval (Plus e1 e2) = eval e1 + eval e2
eval (Minus e1 e2) = eval e1 - eval e2
eval (Times e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 / eval e2
eval (Literal n) = n

-- Test expressions (for internal testing/debugging)
test1 = Plus (Literal 3.0) (Literal 2.0)
test2 = Plus (Literal 3.0) (Div (Literal 1.0) (Literal 2.0))
test3 = Plus (Times (Literal 3.0) (Literal 5.0)) (Div (Literal 1.0) (Literal 2.0))
