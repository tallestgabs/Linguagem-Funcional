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
executeP (Prog fs) =  fst (eval [] (updatecF [] fs) (expMain fs))
    where expMain (f:xs) 
              | (getName f == (Ident "main")) =  getExp f
              | otherwise = expMain xs                                            
          
   
eval :: FCContext -> RContext -> Exp -> (Valor, FCContext)
eval fcc rc x = case x of
    ECon exp0 exp  -> let (v1, fcc1) = eval fcc rc exp0
                          (v2, fcc2) = eval fcc1 rc exp
                      in (ValorStr (s v1 ++ s v2), fcc2)

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

    ECall id lexp -> 
        let 
            -- Função auxiliar
            evalArgs :: FCContext -> RContext -> [Exp] -> ([Valor], FCContext)
            evalArgs currentFcc _ [] = ([], currentFcc)
            evalArgs currentFcc ctx (e:es) =
                let (v, fccNext) = eval currentFcc ctx e
                    (vs, fccFinal) = evalArgs fccNext ctx es
                in (v:vs, fccFinal)

            -- 1. Avalia argumentos
            (evaluatedArgs, fccAfterArgs) = evalArgs fcc rc lexp
            
            -- 2. Cria chave
            cacheKey = FCEA id evaluatedArgs
        in
            -- 3. Verifica Cache
            case lookupShallowFC fccAfterArgs cacheKey of
                -- Caso A: Achou
                Found v -> (v, fccAfterArgs)
                
                -- Caso B: Não achou (Erro no cache)
                NotFound _ -> 
                    let 
                        (ValorFun funDef) = lookup rc id
                        params = getParams funDef
                        
                        paramBindings = zip params evaluatedArgs
                        
                        contextFunctions = filter (\(_,v) -> case v of 
                                                                ValorFun _ -> True 
                                                                _ -> False
                                                  ) rc
                        
                        -- O novo contexto
                        funcBodyRc = paramBindings ++ contextFunctions
                        
                        -- Executa
                        (resultVal, fccAfterBody) = eval fccAfterArgs funcBodyRc (getExp funDef)
                        
                        -- Atualiza
                        finalFcc = (cacheKey, resultVal) : fccAfterBody
                    in
                        (resultVal, finalFcc)
                      


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
             } deriving Eq -- para FunCallEA funcione com Eq (lookup)


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
type FCContext = [(FunCallEA, Valor)]

-- maybe para buscar no cache
data CacheResult a = Found a | NotFound String deriving (Eq, Ord, Show, Read)

-- buscar no cache
lookupShallowFC :: FCContext -> FunCallEA -> CacheResult Valor
lookupShallowFC [] s = NotFound "Not in cache"
lookupShallowFC ((i,v):cs) s 
   | i == s = Found v  -- por causa desse == que FunCallEA precisa ter deriving
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

