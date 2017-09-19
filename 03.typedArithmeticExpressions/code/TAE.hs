module TAE where

import Prelude hiding(True,False)
import Test.HUnit

data Type = TBool 
          | TNat
 deriving(Eq, Show)          

data Exp = True
         | False
         | IfThenElse Exp Exp Exp
         | Zero
         | Succ Exp
         | Pred Exp
         | IsZero Exp
 deriving(Eq, Show)

data Value = Num Exp
           | Bool Exp 
 deriving(Eq, Show)

eval :: Exp -> Value 
eval True = Bool True
eval False = Bool False
eval Zero = Num Zero

eval (Succ exp) = Num (Succ n)
 where
   (Num n) = eval exp

eval (Pred Zero) = Num Zero
eval (Pred (Succ n)) = Num n
eval (Pred exp) = Num n
 where 
   (Num n) = eval exp
   
eval (IfThenElse exp1 exp2 exp3) =
  if (eval exp1 == Bool True)
  then eval exp2
  else eval exp3 

eval (IsZero exp) =
  if (eval exp == Num Zero) then Bool True else Bool False 

typeChecker :: Exp -> Maybe Type
typeChecker True  = Just TBool
typeChecker False = Just TBool
typeChecker Zero  = Just TNat
typeChecker (Succ exp) =
  typeChecker exp >>= \t ->
  if t == TNat then Just TNat else Nothing 
typeChecker (Pred exp) =
  typeChecker exp >>= \t -> 
  if t == TNat then Just TNat else Nothing
typeChecker (IfThenElse c exp1 exp2) =
  typeChecker c    >>= \t1 ->
  typeChecker exp1 >>= \t2 ->
  typeChecker exp2 >>= \t3 ->
  if (t1 == TBool) && t2 == t3 then Just t2 else Nothing 
typeChecker (IsZero exp) = 
  typeChecker exp >>= \t ->
  if (t == TNat) then Just TBool else Nothing 
