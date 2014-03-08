module Interpreter where

import Parser

eval :: Expr -> Integer
eval (Num n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)
