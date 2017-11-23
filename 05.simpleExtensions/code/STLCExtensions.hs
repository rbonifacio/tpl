{-|
Module      : STLCExtensions
Description : Simple Extensions to the Simply Typed Lambda Calculus
Copyright   : (c) rbonifacio, 2017
License     : GPL-3
Maintainer  : rbonifacio@unb.br

An implementation of several extensions to the Simply Typed Lambda
Calculus, as detailed in the book Types and Programming Languages
(B. Pierce).

Contributors: 
--------------------------

(-) Leomar Camargo
(-) Luisa Sinzker 
-}

module STLCExtensions where

import Prelude hiding (lookup)

type Id = String

type Gamma = [(Id, Type)]

type Label = String

type RItem = (Label, Term)

data Type = TBool
          | TInt
          | TString
          | TUnit
          | TRecord [Maybe Type]
          | TArrow Type Type
     deriving(Eq, Show)

data Term = Var Id
          | Lambda (Id, Type) Term
          | App Term Term
          | Let Id Term Term
          | B Bool
          | N Int
          | S String
          | Unit
          | Record [RItem]
          | Projection Term Label
          | IfThenElse Term Term Term
          | Add Term Term
          | Term
          | Seq Term Term
          | Ascribe Term Type
        deriving(Eq, Show)

data Value = VBool Bool
           | VInt Int
           | VString String
           | VUnit
           | VRecord [RItem]
           | VFunction (Id, Type) Term
           | VAscription (Term, Type)
        deriving(Eq, Show)

interp :: Term -> Value
interp (Var x)            = error "cannot evaluate"
interp (N n)              = VInt n
interp (B b)              = VBool b
interp (S s)              = VString s
interp Unit               = VUnit
interp (Lambda (x,t) t1)  = VFunction (x,t) t1
interp (Ascribe x t)      = VAscription (x, t)
interp (App t1 t2)        =
  let v = interp t1
  in case v of
    VFunction(x,t) e -> interp(subst x t2 e)
    otherwise -> error "not a lambda expression"

interp (Let x t1 t2)      = interp (subst x t1 t2)
interp (IfThenElse c t1 t2)
  | c == B True = interp t1
  | otherwise = interp t2
interp (Add t1 t2)        =
  let
    (VInt v1) = interp t1
    (VInt v2) = interp t2
  in VInt (v1 + v2)

interp (Seq t1 t2)        =
    let x = interp t1
    in interp t2

subst :: Id -> Term -> Term -> Term
subst var exp (N n) = N n
subst var exp (B b) = B b
subst var exp (S s) = S s
subst var exp (Unit) = Unit
subst var exp (Add t1 t2) = Add (subst var exp t1) (subst var exp t2)
subst var exp (Lambda arg body) = Lambda arg (subst var exp body)
subst var exp (App t1 t2) = App (subst var exp t1) (subst var exp t2)
subst var exp (Let subId namedExp body)
  | var == subId = (Let subId (subst var exp namedExp) body)
  | otherwise = (Let subId (subst var exp namedExp) (subst var exp body))
subst var exp (Var x)
  | var == x = exp
  | otherwise = (Var x)
subst var exp (IfThenElse c t1 t2) = IfThenElse (subst var exp c)(subst var exp t1)(subst var exp t2)
subst var exp (Seq t1 t2) = Seq (subst var exp t1)(subst var exp t2)
subst var exp (Ascribe x t) = Ascribe(subst var exp x) t


-- | The type checker function. It returns either a
-- type (when the expression is well typed) or Nothing,
-- in the case the expression is ill typed.
(|-) :: Gamma -> Term -> Maybe Type

-- a definition of the type checker function for each expression.

gamma |- (B b)              = Just TBool

gamma |- (N n)              = Just TInt

gamma |- (S s)              = Just TString

gamma |- Unit               = Just TUnit

gamma |- (Var v)            = lookup v gamma >>= \t1 -> Just t1

gamma |- (Let v e1 e2)      = gamma          |- e1 >>= \t1 ->
                              ((v,t1):gamma) |- e2 >>= \t2 ->
                              Just t2

gamma |- (Record items)     = let res = map (\(l,t) -> gamma |- t) items
                              in Just (TRecord res)

gamma |- (Lambda (x, t1) t) = ((x,t1):gamma) |- t >>= \t2 -> return (TArrow t1 t2)

gamma |- (App e1 e2)        = gamma |- e1 >>= \t1 ->
                              gamma |- e2 >>= \t2 ->
                              case t1 of
                                (TArrow t11 t12) -> if t12 == t2 then Just t12 else Nothing
                                otherwise -> Nothing

gamma |- (IfThenElse c t e) = gamma |- c >>= \t1 ->
                              gamma |- t >>= \t2 ->
                              gamma |- e >>= \t3 ->
                              if(t1 == TBool && t2 == t3) then (Just t2) else Nothing


gamma |- (Add e1 e2)        = gamma |- e1 >>= \t1 ->
                              gamma |- e2 >>= \t2 ->
                              if(t1 == TInt && t2 == TInt) then return TInt else Nothing

gamma |- (Seq e1 e2)        = gamma |- e1 >>= \t1 ->
                              gamma |- e2 >>= \t2 ->
                              if t1 == TUnit then return t2 else Nothing

gamma |- (Ascribe e1 t)    = gamma |- e1 >>= \t1 ->
                              if t1 == t then return t else Nothing

sure :: Maybe Type -> Type
sure (Just x) = x
sure Nothing = error "'Nothing' detected"

-- | A lookup function. It searches for a specific
-- mapping involving an identifier and a type.
lookup :: Id -> Gamma -> Maybe Type
lookup k [] = Nothing
lookup k ((v, t):tail)
 | k == v = Just t
 | otherwise = lookup k tail
