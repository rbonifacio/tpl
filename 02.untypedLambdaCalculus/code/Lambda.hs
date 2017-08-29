module Lambda where

type Id = String 

data Term = Var Id 
          | Lambda Id Term
          | App Term Term
 deriving(Read, Eq)

instance Show Term where 
 show (Var x) = x 
 show (Lambda var term) = "\\" ++ var ++ " -> " ++ show term 
 show (App term1 term2) = show term1 ++ show term2

eval :: Term -> Term
eval (Var v)             = undefined
eval (Lambda v body)     = Lambda v body
eval (App t1 t2)         = eval (subst var t2 body)
 where (Lambda var body) = eval t1   
  
subst :: Id -> Term -> Term -> Term
subst v t (Var x)
 | v == x = t
 | otherwise = Var x

subst v t1 (Lambda x t2)
 | v == x = Lambda x t2
 | otherwise = Lambda x (subst v t1 t2)

subst v t1 (App t2 t3) = App (subst v t1 t2) (subst v t1 t3)

-- a, b, c :: String
-- [a, b, c] = ["a", "b", "c"] 

-- test :: Term
-- test = Lambda a (Lambda b (Lambda c (((Var a) (Var b)) Var c)))

-- true :: Term
-- true = Lambda a (Lambda b (Var a)) 

-- false :: Term
-- false = Lambda a (Lambda (Var b)) 
