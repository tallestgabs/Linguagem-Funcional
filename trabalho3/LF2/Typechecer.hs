module Typechecer where

import AbsLF
import Prelude hiding (lookup)
import PrintLF
import Control.Monad (ap, liftM, zipWithM_)

data R a = OK a | Erro String                                   
         deriving (Eq, Ord, Show, Read)

instance Functor R where 
    fmap = liftM 

instance Applicative R where
    pure  = OK 
    (<*>) = ap 

instance Monad R where 
    (OK x) >>= f      = f x
    (Erro msg) >>= _  = Erro msg


isError :: R a -> Bool 
isError (Erro _) = True
isError _        = False


type TContext = [(Ident,Type)]



                                  
tke :: TContext -> Exp -> Type -> R TContext
tke tc exp tp = do 
    tipoInferido <- tinf tc exp 
    if tipoInferido == tp 
        then return tc 
        else Erro ("@typechecker: a expressao "++ printTree exp ++
                   " tem tipo " ++ printTree tipoInferido ++
                   " mas o tipo esperado eh " ++ printTree tp)

                              
tinf :: TContext -> Exp -> R Type
tinf tc x  =  case x of
    -- literais
    EStr str       -> return TStr  
    ETrue          -> return Tbool 
    EFalse         -> return Tbool  
    EInt n         -> return Tint  
    EVar id        -> lookup tc id

    ECon exp0 exp  -> combChecks tc exp0 exp TStr
    EAdd exp0 exp  -> combChecks tc exp0 exp Tint
    ESub exp0 exp  -> combChecks tc exp0 exp Tint
    EMul exp0 exp  -> combChecks tc exp0 exp Tint
    EDiv exp0 exp  -> combChecks tc exp0 exp Tint
    EOr  exp0 exp  -> combChecks tc exp0 exp Tbool
    EAnd exp0 exp  -> combChecks tc exp0 exp Tbool

    ENot exp       -> do 
        tke tc exp Tbool 
        return Tbool
    

    EIf exp expT expE -> do 
        tke tc exp Tint 
        typeThen <- tinf tc expT 
        typeElse <- tinf tc expE 

        if typeThen == typeElse
            then return typeThen
            else Erro ("@typechecker: Tipos incompativeis no IF. THEN: " ++ 
                       printTree typeThen ++ ", ELSE: " ++ printTree typeElse)
                             

    ECall id lexp   -> do 
        tipoFuncao <- lookup tc id 
        case tipoFuncao of 
            TFun tR pTypes -> do 
                if length pTypes /= length lexp 
                    then Erro "@typechecker: numero incorreto de argumentos"
                    else do 
                        zipWithM_ (tke tc) lexp pTypes
                        return tR 
            _ -> Erro ("@typechecker: " ++ printTree id ++ " nao eh uma funcao")



                             
combChecks :: TContext -> Exp -> Exp -> Type -> R Type
combChecks tc exp1 exp2 tp = do 
    tke tc exp1 tp 
    tke tc exp2 tp 
    return tp
                             

lookup :: TContext -> Ident -> R Type
lookup [] id = Erro ("@typechecker: " ++ printTree id ++ " nao esta no contexto. ")
lookup ((id,value):cs) key
   | id == key = OK value
   | otherwise = lookup cs key


typeCheckP :: Program  -> [R TContext]
typeCheckP (Prog fs) = 
    case updatecF [] fs of 
        OK ctx -> map (typeCheckF ctx) fs
        Erro msg -> [Erro msg]

                                                
typeCheckF ::  TContext -> Function -> R TContext    
typeCheckF tc (Fun tR _ decls exp) = 
    tke (parameterTypeBindings ++ functionTypes) exp tR 
      where parameterTypeBindings = map (\(Dec tp id) -> (id, tp)) decls 
            functionTypes = filter (\(_,t) -> case t of TFun _ _ -> True; _ -> False) tc


updateTC :: TContext -> Ident -> Type -> R TContext
updateTC [] id tp = return [(id,tp)]
updateTC ((id,tp):idTps) idN tpN 
  | id == idN = Erro ("@typechecker: identificador" ++ printTree id ++ " nao pode ter mais de um tipo")
  | otherwise = do 
      restOK <- updateTC idTps idN tpN 
      return ((id, tp) : restOK)


updatecF :: TContext -> [Function] -> R TContext
updatecF tc [] = return tc
updatecF tc (f@(Fun _ nomeF _ _):fs) = do 
    tcNew <- updateTC tc nomeF (getFunctionType f)
    updatecF tcNew fs
                                                  

getFunctionType :: Function -> Type
getFunctionType (Fun tipoRetorno _ decls _) = TFun tipoRetorno (map (\(Dec tp _ )-> tp) decls)
                                                     
                                                     
                                                     


