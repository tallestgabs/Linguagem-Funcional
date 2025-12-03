{-# LANGUAGE DeriveDataTypeable #-}
module Optimizer where

import AbsLF
import Interpreter
import Data.Generics -- SYB
import Control.Monad.Writer
import Control.Monad.State

type OptM a = Writer [String] a

optimizeP :: Program -> (Program, [String])
optimizeP p = runWriter (everywhereM (mkM optimizeEM) p)
       
optimizeEM :: Exp -> OptM Exp
optimizeEM exp  = case exp of
                      EIf (EInt n) expT expE ->
                          if n /= 0
                          then do
                              tell ["Optimizing IF for THEN"]                       
                              return expT

                          else do 
                              tell ["Optimizing IF for ELSE"]
                              return expE

                      
                      EIf (ETrue) expT _     -> do
                          tell ["Optimizing IF (TRUE)"]
                          return expT

                      EIf (EFalse) _ expE    -> do
                          tell ["Optimizing IF (FALSE)"] 
                          return expE

                      _ -> if isGround exp
                              then do 
                                  let (val, _) = eval [] [] exp 
                                  let newValue = wrapValueExpression val

                                  if exp /= newValue
                                     then tell ["Constant Folding: " ++ show exp ++ " -> " ++ show newValue]
                                     else return () -- se ja era literal nao faz nada
                                  
                                  return newValue 

                              else return exp

                        
isGround:: Exp -> Bool 
isGround expr = everything (&&) (extQ (const True) check) expr
  where
    check :: Exp -> Bool 
    check (EVar _)    = False
    check (ECall _ _) = False 
    check _           = True



wrapValueExpression :: Valor -> Exp 
wrapValueExpression (ValorInt i)      = EInt i
wrapValueExpression (ValorStr s)      = EStr s 
wrapValueExpression (ValorBool True)  = ETrue
wrapValueExpression (ValorBool False) = EFalse
                       