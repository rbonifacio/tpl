module RBL where

import Data.Bool

type Id = String

data Expression = B Bool 
                | N Integer
                | Add Expression Expression 
                | Sub Expression Expression
                | And Expression Expression
                | Or  Expression Expression
                | Not Expression
                | IfThenElse Expression Expression Expression     
                | Let Id Expression Expression    
                | Ref Id                                          -- symbol (or variable)   
                | Lambda Id Expression                            -- abstraction
                | App Expression Expression                       -- application
               deriving(Show, Eq)  


eval :: Expression -> Expression
eval (B b)                    = (B b)
eval (N n)                    = N n
eval (Add e1 e2)              = numBinExpression  (+) e1 e2
eval (Sub e1 e2)              = numBinExpression  (-) e1 e2 
eval (And e1 e2)              = boolBinExpression (&&) e1 e2
eval (Or e1 e2)               = boolBinExpression (||) e1 e2
eval (IfThenElse c e1 e2)     = if (eval c == (B True)) then eval e1 else eval e2
eval (Let v e1 e2)            = eval (substitution v e1 e2)  
eval (Ref v)                  = undefined 
eval (Lambda v e)             = (Lambda v e) 
eval (App e1 e2)              = eval (substitution x (eval e2) e) -- inner most evaluation strategy 
 where (Lambda x e) = eval e1

-- auxiliarly functions

substitution :: Id -> Expression -> Expression -> Expression
substitution _ _ (B v)                 = B v
substitution _ _ (N v)                 = N v
substitution v e (Add e1 e2)           = Add (substitution v e e1) (substitution v e e1)
substitution v e (Sub e1 e2)           = Sub (substitution v e e1) (substitution v e e1)
substitution v e (And e1 e2)           = And (substitution v e e1) (substitution v e e1)
substitution v e (Or e1 e2)            = Or (substitution v e e1) (substitution v e e1)
substitution v e (IfThenElse e1 e2 e3) = IfThenElse (substitution v e e1) (substitution v e e2) (substitution v e e3)
substitution v e (Let x e1 e2)
 | v == x                              = Let x (substitution v e e1) e2
 | otherwise                           = Let x (substitution v e e1) (substitution v e e2) 
substitution v e (Ref x)
 | v == x                              = e
 | otherwise                           = (Ref x)
substitution v e (Lambda x exp)
  | v == x                             = Lambda v exp
  | otherwise                          = Lambda x (substitution v e exp) 

 
numBinExpression :: (Integer -> Integer -> Integer) -> Expression -> Expression -> Expression
numBinExpression op e1 e2 =
  let
    (N v1) = eval e1
    (N v2) = eval e2
  in N (v1 `op` v2)

boolBinExpression :: (Bool -> Bool -> Bool) -> Expression -> Expression -> Expression
boolBinExpression op e1 e2 =
  let
    (B v1) = eval e1
    (B v2) = eval e2
  in B (v1 `op` v2)
  
  
  
