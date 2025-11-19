module Interpreter where

import AbsLF
import Prelude hiding (lookup)
            
getName :: Function -> Ident
getName (Fun _ name _ _) = name

getParams :: Function -> [Ident]
getParams (Fun _ _ decls _) = map (\(Dec _ id) -> id) decls

getExp :: Function -> Exp 
getExp (Fun _ _ _ exp) = exp


executeP :: Program -> Valor

-- agora executeP vai iniciar um cache []
executeP (Prog fs) =  eval (updatecF [] fs, []) (expMain fs)
    where expMain (f:xs) 
              | (getName f == (Ident "main")) =  getExp f
              | otherwise = expMain xs                                            
          
   
eval :: FCContext -> RContext -> Exp -> (Valor, FCContex)
eval context x = case x of
    ECon exp0 exp  -> ValorStr ( s (eval context exp0) ++  s (eval context exp) )
    EAdd exp0 exp  -> ValorInt ( i (eval context exp0)  +  i (eval context exp))
    ESub exp0 exp  -> ValorInt ( i (eval context exp0)  -  i (eval context exp)) 
    EMul exp0 exp  -> ValorInt ( i (eval context exp0)  *  i (eval context exp))
    EDiv exp0 exp  -> ValorInt ( i (eval context exp0) `div` i (eval context exp)) 
    EOr  exp0 exp  -> ValorBool ( b (eval context exp0)  || b (eval context exp))
    EAnd exp0 exp  -> ValorBool ( b (eval context exp0)  && b (eval context exp))
    ENot exp       -> ValorBool ( not (b (eval context exp)))
    EStr str       -> ValorStr str
    ETrue          -> ValorBool True
    EFalse         -> ValorBool False
    EInt n         -> ValorInt n
    EVar id        -> lookup context  id

    EIf exp expT expE -> if(i (eval context exp) /= 0)
      then eval context expT
      else eval context expE

    ECall id lexp   -> eval (paramBindings ++ contextFunctions) (getExp funDef)
                          where (ValorFun funDef) = lookup context id
                                parameters =  getParams funDef
                                paramBindings = zip parameters (map (eval context) lexp)
                                contextFunctions = filter (\(i,v) -> case v of 
                                                                         ValorFun _ -> True 
                                                                         _ -> False
                                                           ) 
                                                          context
                                                          
    


data Valor = ValorInt {
               i :: Integer         
             }
            | 
             ValorFun {
               f :: Function
             }   
            | 
             ValorStr {
               s :: String
             } 
            | ValorBool {
               b :: Bool
             } deriving (Eq, Show) -- para FunCallEA funcione com Eq (lookup)

instance Show Valor where
  show (ValorBool b) = show b
  show (ValorInt i) = show i
  show (ValorStr s) = s
  show (ValorFun f) = show f 
--(\(Ident x) -> x) nf

type RContext = [(Ident,Valor)]

--"Function Call with Evaluated Arguments" -> Chave do cache
data FunCallEA = FCEA Ident [Valor] deriving (Eq, Show)

-- cache de memoizacao
type FCContex = [(FunCallEA, Valor)]

-- maybe para buscar no cache
data R a = OK a | Erro String deriving (Eq, Ord, Show, Read)

-- buscar no cache
lookupShallowFC :: FCContex -> FunCallEA -> R Valor
lookupShallowFC [] s = Erro "Not in cache"
lookupShallowFC ((i,v):cs) s 
   | i == s = OK v  -- por causa desse == que FunCallEA precisa ter deriving
   | otherwise = lookupShallowFC cs s

lookup :: RContext -> Ident -> Valor
lookup ((i,v):cs) s
   | i == s = v
   | otherwise = lookup cs s

update :: RContext -> Ident -> Valor -> RContext
update [] s v = [(s,v)]
update ((i,v):cs) s nv
  | i == s = (i,nv):cs
  | otherwise = (i,v) : update cs s nv


updatecF :: RContext -> [Function] -> RContext
updatecF c [] = c
updatecF c (f:fs) = updatecF (update c (getName f) (ValorFun f)) fs

