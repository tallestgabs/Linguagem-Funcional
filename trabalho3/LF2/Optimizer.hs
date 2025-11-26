module Optimizer where

import AbsLF
import Interpreter
import Data.Generics -- SYB

optimizeP :: Program -> Program
optimizeP = everywhere (mkT optimizeE)
       
optimizeE :: Exp -> Exp
optimizeE exp  = case exp of

                      EAdd EInt(n1) EInt(n2) -> EInt(n1 + n2) 
                      ESub EInt(n1) EInt(n2) -> EInt(n1 - n2)
                      EMul EInt(n1) EInt(n2) -> EInt(n1 * n2)                       
                      EDiv EInt(n1) EInt(n2) 
                        | n2 /= 0 -> EInt(n1 `div` n2)

                      EOr (ETrue) _          -> ETrue 
                      EOr _ (ETrue)          -> ETrue 
                      EOr (EFalse) (EFalse)  -> EFalse

                      EAnd (ETrue) (ETrue)   -> ETrue 
                      EAnd (EFalse) _        -> EFalse
                      EAnd _ (EFalse)        -> EFalse

                      ENot (ETrue)           -> EFalse 
                      ENot (EFalse)          -> ETrue

                      ECon EStr(s1) EStr(s2) -> EStr(s1 ++ s2)
                      EIf EInt(n) expT expE
                        | n /= 0             -> expT                          
                        | otherwise          -> expE
                      
                      EIf (ETrue) expT _     -> expT
                      EIf (EFalse) _ expE    -> expE

                      _ -> exp 