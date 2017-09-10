module TAE where

import Prelude hiding(True,False)
import Test.HUnit

data Type = TBool 
          | TNat 

data Exp = True
         | False
         | IfThenElse Exp Exp Exp
         | Zero
         | Succ Exp
         | Pred Exp
         | IsZero Exp
 deriving(Eq, Show)


eval :: Exp -> Exp
eval True = True
eval False = False
eval Zero = Zero 
eval (Succ exp) = Succ (eval exp)
eval (Pred Zero) = Zero
eval (Pred (Succ exp)) = eval exp
eval (Pred exp) = Pred (eval exp)
eval (IfThenElse True exp2 exp3) = eval exp2
eval (IfThenElse False exp2 exp3) = eval exp3
eval (IfThenElse exp1 exp2 exp3) = IfThenElse (eval exp1) exp2 exp3
eval (IsZero Zero) = True
eval (IsZero (Succ exp)) = False
eval (IsZero exp) = IsZero (eval exp) 

typeChecker :: Exp -> Type
typeChecker = undefined 
