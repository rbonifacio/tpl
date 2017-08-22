module RBL where

type Id = String

data Expression = B Bool 
                | N Integer
                | Add Expression Expression 
                | Sub Expression Expression
                | And Expression Expression
                | Or  Expression Expression
                | Not Expression
                | IfThenElse Expression Expression Expression
                | Ref Id                                        -- symbol  
                | Lambda Id Expression                          -- abstraction
                | App Expression Expression                     -- application


eval :: Expression -> Expression
eval (B b)                 = (B b)
eval (Num n)               = Num n
eval (Add e1 e2)           = numBinExpression  (+) e1 e2
eval (Sub e1 e2)           = numBinExpression  (-) e1 e2 
eval (And e1 e2)           = boolBinExpression (&&) e1 e2
eval (Or e1 e2)            = boolBinExpression (||) e1 e2
eval (IfThenElse e1 e2)    = if (eval c == (B True)) then eval e1 else eval e2 
eval (Ref v)               = undefined 
eval (Lambda v e)          = (Lambda v e) 
eval (App e1 e2)           =
  let (Lambda x e) = eval e1
  in eval (substitution x (eval e2) e) 
 
-- auxiliarly functions

substitution :: Id -> Expression -> Expression -> Expression
substitution _ _ (B v)                 = B v
substitution _ _ (N v)                 = N v
substitution v e (Add e1 e2)           = Add (substitution v e e1) (substitution v e e1)
substitution v e (Sub e1 e2)           = Sub (substitution v e e1) (substitution v e e1)
substitution v e (And e1 e2)           = And (substitution v e e1) (substitution v e e1)
substitution v e (Or1 e2)              = Or (substitution v e e1) (substitution v e e1)
substitution v e (IfThenElse e1 e2 e3) = undefined
substitution v e (RefId var)           = undefined 
substitution v e (Lambda var exp)      = undefined 

 


numBinExpression :: (Int -> Int -> Int) -> Expression -> Expression -> Expression
numBinExpression op e1 e2 =
  let
    (Num v1) = eval e1
    (Num v2) = eval v2
  in Num (v1 `op` v2)

boolBinExpression :: (Bool -> Bool -> Bool) -> Expression -> Expression -> Expression
boolBinExpression op e1 e2 =
  let
    (Bool v1) = eval e1
    (Bool v2) = eval v2
  in Bool (v1 `op` v2)
  
  
  
