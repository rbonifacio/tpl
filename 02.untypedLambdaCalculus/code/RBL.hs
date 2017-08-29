{-|
Module      : RBL
Description : The definition of the RBL-Calculus in Haskell 
Copyright   : (c) Rodrigo Bonifacio, 2017
License     : MIT 
Maintainer  : rbonifacio@unb.br
Stability   : experimental


The RBL-Calculus is an extension of the Lambda Calculus 
discussed in the Declarative Languages graduate course 
at University of Brasilia.  
-}

module RBL (Expression(..), eval) where

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

-- | The 'eval' function defines the operational semantics of our language. 
-- It reduces an expression either to a number, a bool or a lambda expression. 
-- There is an statement to each type of expression of our language. 

eval :: Expression -> Expression
eval (B b)                    = B b
eval (N n)                    = N n
eval (Add e1 e2)              = numBinExpression  (+) e1 e2
eval (Sub e1 e2)              = numBinExpression  (-) e1 e2 
eval (And e1 e2)              = boolBinExpression (&&) e1 e2
eval (Or e1 e2)               = boolBinExpression (||) e1 e2
eval (Not e)                  = let (B b) = eval e in B (not b)
eval (IfThenElse c e1 e2)     = if (eval c == (B True)) then eval e1 else eval e2
eval (Let v e1 e2)            = eval (subst v e1 e2)  
eval (Ref v)                  = error "we should not evaluate a free variable"
eval (Lambda v e)             = (Lambda v e) 
eval (App e1 e2)              = eval (subst x e2 e)  
 where (Lambda x e) = eval e1

-- | The 'subst' function substitues an id by an expression within an expression body. 
-- Consider as example an expression like let x = e1 in e2. We use this function 
-- to replace x by e1 within e2. Therefore, this function takes an id and two 
-- expressions, and returns another expression. 

subst :: Id -> Expression -> Expression -> Expression
subst _ _ (B v)                 = B v
subst _ _ (N v)                 = N v
subst v e (Add e1 e2)           = Add (subst v e e1) (subst v e e2)
subst v e (Sub e1 e2)           = Sub (subst v e e1) (subst v e e2)
subst v e (And e1 e2)           = And (subst v e e1) (subst v e e2)
subst v e (Or e1 e2)            = Or (subst v e e1) (subst v e e2)
subst v e (Not e1)              = Not (subst v e e1)  
subst v e (IfThenElse e1 e2 e3) = IfThenElse (subst v e e1) (subst v e e2) (subst v e e3)
subst v e (Let x e1 e2)
 | v == x                       = Let x (subst v e e1) e2
 | otherwise                    = Let x (subst v (eval e) e1) (subst v e e2) 
subst v e (Ref x)
 | v == x                       = e
 | otherwise                    = (Ref x)
subst v e (Lambda x exp)
  | v == x                      = Lambda v exp
  | otherwise                   = Lambda x (subst v e exp) 


-- |* auxiliary functions 
 
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
  
