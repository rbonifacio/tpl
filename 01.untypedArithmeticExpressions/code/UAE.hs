module UAE where

import Prelude hiding(True,False)
import Test.HUnit

data Exp = True
         | False
         | IfThenElse Exp Exp Exp
         | Zero
         | Succ Exp
         | Pred Exp
         | IsZero Exp
 deriving(Eq, Show)

consts :: Exp -> [Exp]
consts True = [True]
consts False = [False]
consts Zero = [Zero]
consts (IfThenElse exp1 exp2 exp3) = consts exp1 ++ consts exp2 ++ consts exp3
consts (Succ exp) = consts exp
consts (Pred exp) = consts exp
consts (IsZero exp) = consts exp


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

s = Succ(IfThenElse True (Succ Zero) (Succ (Succ Zero)))

tc01 = TestCase (assertEqual "test for consts(True)" (consts True) [True])
tc02 = TestCase (assertEqual "consts test for succ(Zero)" (consts (Succ Zero)) [Zero])
tc03 = TestCase (assertEqual "consts test for s" (consts s) [True, Zero, Zero]) 
