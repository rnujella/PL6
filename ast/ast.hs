module Ast where

-- Define the expression data type
data Expr = Plus Expr Expr 
          | Minus Expr Expr 
          | Times Expr Expr 
          | Div Expr Expr
          | Literal Float

-- Implement the eval function
eval :: Expr -> Float
eval (Plus e1 e2) = eval e1 + eval e2
eval (Minus e1 e2) = eval e1 - eval e2
eval (Times e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 / eval e2
eval (Literal n) = n
