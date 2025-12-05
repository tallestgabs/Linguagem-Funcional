module Optimizer where

import AbsLF
import Interpreter
import Data.Generics -- SYB

optimizeP :: Program -> Program
optimizeP = everywhere (mkT optimizeE)
       
optimizeE :: Exp -> Exp
optimizeE exp  = case exp of
                      EIf (EInt n) expT expE
                        | n /= 0             -> expT                          
                        | otherwise          -> expE
                      
                      EIf (ETrue) expT _     -> expT
                      EIf (EFalse) _ expE    -> expE

                      _ -> if(isGround exp)
                              then wrapValueExpression  (fst (eval [] [] exp))
                              else exp

                        
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