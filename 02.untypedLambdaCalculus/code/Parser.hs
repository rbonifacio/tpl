module Parser where

import Lambda

import Text.ParserCombinators.Parsec



term :: GenParser Char st Term
term =
  do spaces
     t1 <- term
     spaces 
     t2 <- term
     spaces
     return (App t1 t2)
  <|>    
  do spaces
     char '('
     spaces
     v <- many1 letter
     spaces 
     string "->"
     spaces  
     e <- term
     spaces
     char ')'
     return (Lambda v e)
  <|>
  do spaces
     v <- many1 letter
     return (Var v)
 

parseTerm :: String -> Either ParseError Term
parseTerm input = parse term "unknown" input 
