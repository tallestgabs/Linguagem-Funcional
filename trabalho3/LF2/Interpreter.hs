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
eval fcc rc x = case x of
    ECon exp0 exp  -> let (v1, fcc1) = eval fcc rc exp0
                          (v2, fcc2) = eval fcc1 rc exp
                      in (ValorInt (s v1 ++ s v2), fcc2)

    EAdd exp0 exp  -> let (v1, fcc1) = eval fcc rc exp0
                          (v2, fcc2) = eval fcc1 rc exp
                      in (ValorInt (i v1 + i v2), fcc2)

    ESub exp0 exp  -> let (v1, fcc1) = eval fcc rc exp0
                          (v2, fcc2) = eval fcc1 rc exp
                      in (ValorInt (i v1 - i v2), fcc2)

    EMul exp0 exp  -> let (v1, fcc1) = eval fcc rc exp0
                          (v2, fcc2) = eval fcc1 rc exp
                      in (ValorInt (i v1 * i v2), fcc2)

    EDiv exp0 exp  -> let (v1, fcc1) = eval fcc rc exp0
                          (v2, fcc2) = eval fcc1 rc exp
                      in (ValorInt (i v1 `div` i v2), fcc2)

    EOr  exp0 exp  -> let (v1, fcc1) = eval fcc rc exp0 
                          (v2, fcc2) = eval fcc1 rc exp
                      in (ValorBool (b v1 || b v2), fcc2)

    EAnd exp0 exp  -> let (v1, fcc1) = eval fcc rc exp0 
                          (v2, fcc2) = eval fcc1 rc exp 
                      in (ValorBool (b v1 && b v2), fcc2)

    ENot exp       -> let (v1, fcc1) = eval fcc rc exp 
                      in (ValorBool (not(b v1)), fcc1)

    EStr str       -> (ValorStr str, fcc)
    ETrue          -> (ValorBool True, fcc)
    EFalse         -> (ValorBool False, fcc)
    EInt n         -> (ValorInt n, fcc)
    EVar id        -> (lookup rc id, fcc)

    EIf exp expT expE -> let(vCond, fcc1) = eval fcc rc exp
                         in if(i vCond /= 0)
                            then eval fcc1 rc expT
                            else eval fcc1 rc expE

    ECall id lexp   -> 
        let 
            -- funcao auxiliar para avaliar lista de argumento passado no cache
            evalArgs :: FCContex -> RContext -> [Exp] -> ([Valor], FCContex)
            evalArgs currentFcc _ [] = ([], currentFcc)
            evalArgs currentFcc context (e:es) =
                let (v, fccNext) = eval currentFcc context e 
                    (vs, fccFinal) = evalArgs fccNext context es 
                in (v:vs, fccFinal)

            -- avalia argumentos usando cache atual
            (evaluatedArgs, fccAfterArgs) = evalArgs fcc rc lexp 

            -- cria a key
            cacheKey = FCEA id evaluatedArgs
        in
          -- verifica o cache apos avaliar os argumentos
            case lookupShallowFC fccAfterArgs cacheKey of 
              -- retorna valor e o cache atual
                OK v -> (v, fccAfterArgs)

              -- cache miss
                Erro _ ->
                    let 
                        (ValorFun funDef) = lookup rc id 
                        params = getParams funDef 

                        -- cria RContext apenas para a execucao da funcao
                        paramBindings = zip params evaluatedArgs
                        contextFunctions = filter (\(_,v) -> case v of ValorFun _ -> True; _ -> False) rc 

                        -- novo novo RContext
                        

      
      
      
      
      
      
      
      eval (paramBindings ++ contextFunctions) (getExp funDef)
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

