module STLC where

import Prelude hiding (lookup)

type Id = String 

type Gamma = [(Id, Type)]

data Type = TBool
          | TInt 
          | TArrow Type Type
     deriving(Eq, Show)

data Term = Var Id
          | Lambda (Id, Type) Term
          | App Term Term
          | B Bool
          | N Int
          | IfThenElse Term Term Term
          | Add Term Term 

lookup :: Id -> Gamma -> Maybe Type
lookup k [] = Nothing
lookup k ((v, t):tail)
 | k == v = Just t
 | otherwise = lookup k tail 

(|-) :: Gamma -> Term -> Maybe Type
gamma |- (B b) = Just TBool
gamma |- (N n) = Just TInt

gamma |- (Var v) = lookup v gamma

gamma |- (Lambda (x, t1) t) = Just (TArrow t1 t2)
 where (Just t2) = ((x,t1):gamma) |- t

gamma |- (App t1 t2) = if b == c then Just b else Nothing 
  where
    (Just (TArrow a b)) = gamma |- t1
    (Just c)            = gamma |- t2  

gamma |- (IfThenElse c t e) = if(t1 == TBool && t2 == t3) then (Just t2) else Nothing
  where
   (Just t1) = gamma |- c
   (Just t2) = gamma |- t
   (Just t3) = gamma |- e
    
gamma |- (Add e1 e2) = if(t1 == TInt && t2 == TInt) then Just TInt else Nothing
  where
    (Just t1) = gamma |- e1
    (Just t2) = gamma |- e2
