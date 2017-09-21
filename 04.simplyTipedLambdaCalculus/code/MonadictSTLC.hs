module MonadicSTLC where

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

gamma |- (Lambda (x, t1) t) = ((x,t1):gamma) |- t >>= \t2 -> return (TArrow t1 t2)

gamma |- (App e1 e2) = gamma |- e1 >>= \t1 ->
                       gamma |- e2 >>= \t2 ->
                       case t1 of
                         (TArrow t11 t12) -> if t12 == t2 then Just t12 else Nothing
                         otherwise -> Nothing
                         
gamma |- (IfThenElse c t e) = gamma |- c >>= \t1 ->
                              gamma |- t >>= \t2 ->
                              gamma |- e >>= \t3 ->
                              if(t1 == TBool && t2 == t3) then (Just t2) else Nothing

    
gamma |- (Add e1 e2) = gamma |- e1 >>= \t1 ->
                       gamma |- e2 >>= \t2 ->
                       if(t1 == TInt && t2 == TInt) then return TInt else Nothing

