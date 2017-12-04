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

type TItem = (Term)

data Type = TBool
          | TInt
          | TString
          | TUnit
          | TRecord [(Label,Type)]
          | TTuple [Type]
          | TArrow Type Type
          | TSum Type Type 
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
          | Tuple [TItem]
          | Inl Term
          | Inr Term
          | Case Term (Id, Term) (Id, Term) 
          | TProjection Int Term
          | RProjection Label Term
          | IfThenElse Term Term Term
          | Add Term Term
          | Term
          | Seq Term Term
          | Ascribe Term Type
          | TValue Value
        deriving(Eq, Show)

data Value = VBool Bool
           | VInt Int
           | VString String
           | VUnit
           | VInl Value
           | VInr Value
           | VRecord [(Label, Value)]
           | VTuple [Value]
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
    VFunction(x,t) e -> interp(subst x (interp t2) e)
    otherwise -> error "not a lambda expression"
interp (TValue v) = v

interp (Let x t1 t2)      = interp (subst x (interp t1) t2)
interp (IfThenElse c t1 t2)
  | (interp c) == VBool True = interp t1
  | otherwise = interp t2

interp (Add t1 t2)        =
  let
    (VInt v1) = interp t1
    (VInt v2) = interp t2
  in VInt (v1 + v2)

interp (Seq t1 t2)        =
    let x = interp t1
    in interp t2

interp (Record items)     = let res = map (\(l,t) -> (l, interp t)) items
                             in (VRecord res)
interp (Tuple items)      = let res = map (\t -> interp t) items
                             in (VTuple res)
                                
interp (RProjection l r)  = interp (searchRecord (l) (r))

interp (TProjection i t)  = interp (searchTuple (i) (t))

interp (Inl t) = VInl (interp t)
interp (Inr t) = VInl (interp t)

interp (Case t0 (x1, t1) (x2, t2)) =
  case interp t0 of
    (VInl v) -> interp $ subst x1 v t1
    (VInr v) -> interp $ subst x2 v t2 
  


subst :: Id -> Value -> Term -> Term
subst var v1 (N n) = N n
subst var v1 (B b) = B b
subst var v1 (S s) = S s
subst var v1 (Unit) = Unit
subst var v1 (Add t1 t2) = Add (subst var v1 t1) (subst var v1 t2)
subst var v1 (Lambda arg body) = Lambda arg (subst var v1 body)
subst var v1 (App t1 t2) = App (subst var v1 t1) (subst var v1 t2)
subst var v1 (Let subId namedExp body)
  | var == subId = (Let subId (subst var v1 namedExp) body)
  | otherwise = (Let subId (subst var v1 namedExp) (subst var v1 body))
subst var v1 (Var x)
  | var == x = (TValue v1)
  | otherwise = (Var x)
subst var v1 (IfThenElse c t1 t2) = IfThenElse (subst var v1 c)(subst var v1 t1)(subst var v1 t2)
subst var v1 (Seq t1 t2) = Seq (subst var v1 t1)(subst var v1 t2)
subst var v1 (Ascribe x t) = Ascribe(subst var v1 x) t
subst var v1 (Inl t) = Inl (subst var v1 t)
subst var v1 (Inr t) = Inr (subst var v1 t)
subst var v1 (Record items) = Record $ map (\(l,t) -> (l, subst var v1 t)) items
subst var v1 (RProjection l t) = RProjection l (subst var v1 t)
subst var v1 (Case t0 (x1,t1) (x2, t2)) = Case (subst var v1 t0)
                                               (x1, subst var v1 t1)
                                               (x2, subst var v1 t2) 
subst var v1 (TValue v) = TValue v

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

gamma |- (Record items)     = let res = map (\(l,t) -> (l, sure (gamma |- t))) items                               
                               in Just (TRecord res)

gamma |- (Tuple items)      = let res = map (\t -> sure (gamma |- t)) items
                               in Just (TTuple res)

gamma |- (RProjection l r)  = gamma |- (searchRecord (l) (r)) >>= \t1 -> return (t1)

gamma |- (TProjection i t)  = gamma |- (searchTuple (i) (t)) >>= \t1 -> return (t1)

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
 

-- | A search function to records. It looks for a certain element in
-- the record by its label and returns its value
searchRecord :: Label -> Term -> Term
searchRecord _ (Record [])                  = error "element in Record not found"
searchRecord (x) (Record ((label,item):xs)) = if x == label then item else searchRecord (x) (Record xs)
searchRecord (x) (TValue (VRecord ((label,item):xs))) = if x == label then (TValue item) else searchRecord (x) (TValue (VRecord xs))

-- | A search function to tuples. Its looks for a certain element by index
-- in the tuple and return its value 
searchTuple :: Int -> Term -> Term 
searchTuple _ (Tuple [])        = error "element in Tuple not found"
searchTuple index (Tuple items) = items !! index



-- test cases

r1 = Record [("firstLast", S "walter"), ("address", S "unb")]
r2 = Record [("name", S "leomar"), ("email", S "leomar@unb")]

t1 = sure $ [] |- r1
t2 = sure $ [] |- r2
t3 = (TSum t1 t2) 

getName = Lambda ("a", t3) (Case (Var "a")
                             ("x", RProjection "firstLast" (Var "x"))
                             ("x", RProjection "name" (Var "x")))
