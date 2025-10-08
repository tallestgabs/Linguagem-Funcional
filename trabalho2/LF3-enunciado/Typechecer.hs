module Typechecer where

import AbsLF
import Prelude hiding (lookup)
import PrintLF

data R a = OK a | Erro String                                   
         deriving (Eq, Ord, Show, Read)

isError e = case e of
    OK _ -> False
    Erro _ -> True 


type TContext = [(Ident,Type)]

typeCheckP :: Program  -> [R TContext]
typeCheckP (Prog fs) = let nCtx = updatecF [] fs in
                          case nCtx of
                             OK ctx -> map (typeCheckF ctx) fs
                             Erro msg -> [Erro msg]
                                                  
typeCheckF ::  TContext -> Function -> R TContext    
typeCheckF tc (Fun tR _ decls exp) = tke (parameterTypeBindings ++ functionTypes) exp tR
                                        where parameterTypeBindings = map (\(Dec tp id) -> (id,tp)) decls
                                              functionTypes = filter (\(i,t) -> case t of 
                                                                                 TFun _  _ -> True 
                                                                                 _ -> False
                                                                      ) tc
                                    
tke :: TContext -> Exp -> Type -> R TContext
tke tc exp tp = let r = tinf tc exp in
                          case r of
                             OK tipo -> if (tipo == tp )
                                           then OK tc
                                           else Erro ("@typechecker: a expressao "++ printTree exp ++ " tem tipo " ++ 
                                                     printTree tipo ++ " mas o tipo esperado eh "
                                                     ++ printTree tp)
                             Erro msg -> Erro msg  

tinf :: TContext -> Exp -> R Type
tinf tc x  =  case x of
    ECon exp0 exp  -> combChecks tc exp0 exp TStr
    EAdd exp0 exp  -> combChecks tc exp0 exp Tint
    ESub exp0 exp  -> combChecks tc exp0 exp Tint
    EMul exp0 exp  -> combChecks tc exp0 exp Tint
    EDiv exp0 exp  -> combChecks tc exp0 exp Tint
    EOr  exp0 exp  -> combChecks tc exp0 exp Tbool
    EAnd exp0 exp  -> combChecks tc exp0 exp Tbool
    ENot exp       -> let r = tke tc exp Tbool in 
                         case r of 
                             OK _ -> OK Tbool
                             Erro msg -> Erro msg
    EStr str       -> OK TStr  
    ETrue          -> OK Tbool 
    EFalse         -> OK Tbool  
    EInt n         -> OK Tint  
    EVar id        -> lookup tc id
    eIf@(EIf exp expT expE) -> let r = tke tc exp Tint in
                                 case r of
                                   OK _ -> let r2 = tinf tc expT  in 
                                             case r2 of
                                               OK tExpT -> let r3 = tinf tc expE in 
                                                            case r3 of
                                                              OK tExpE -> if tExpT == tExpE
                                                                             then OK tExpT
                                                                             else Erro ("tipos das expressoes do IF na expressao: " ++ printTree eIf)                                 
                                                              Erro msg -> Erro (msg ++ " na expressao: " ++ printTree eIf)                                                            
                                               Erro msg -> Erro (msg ++ " na expressao: " ++ printTree eIf)
                                   Erro msg -> Erro (msg ++ " na expressao: " ++ printTree eIf)
                                   
    {- TODO: 1)completar abaixo trocando undefined pelo retorno apropriado 
             2) explicar o argumento de tinf abaixo
    -}
    ELambda params exp -> case (tinf (parameterTypeBindings ++ tc) exp) of  -- tenta descobrir o tipo de exp passando como contexto tc (de fora)
                            OK tExp -> OK (TFun tExp paramsType)     -- se tExp for um OK entao temos uma nova funcao com o tExp como tipo de retorno e paramsType como parametros
                              where paramsType = map (\(Dec tp _) -> tp) params  -- definimos paramsType que vai retornar uma lista contendo os tipos dos parametros
                            Erro msg -> Erro msg  -- se nao tiver um OK entao da erro
                           where parameterTypeBindings = map (\(Dec tp id) -> (id,tp)) params  -- define parameterType... que retorna uma lista com (identificador, tipo do parametro)

    {- TODO: 1)completar abaixo trocando undefined pelo retorno apropriado 
             2) fazer as explicacoes necessarias
    -}    
    ECall exp lexp  -> case (tinf tc exp) of  
                        OK (TFun tR pTypes) -> if (length pTypes >= length lexp) -- TODO explicar >=      se fosse == estariamos analisando apenas aplicacao completa, porem com >= alem de completas tambem faremos as aplicacoes parciais
                                                 then 
                                                   if (isThereError tksArgs /= [])  -- se tksArgs tiver pego algum erro a lista vai ser != [], pois os elementos da lista sao erros
                                                    then Erro " @typechecker: tipo incompativel entre argumento e parametro"
                                                    else if (length pTypes > length lexp)  -- TODO o que isso testa ?    aplicacao parcial (tamanho de parametro maior que argumentos passados)
                                                          then OK (TFun tR drop(length lexp) pTypes)  -- se for parcial entao vai ser uma nova funcao com o mesmo tR e faz um drop (remove) dos parametros os argumentos que ja foram passados
                                                          else OK tR -- se nao for parcial entao vai ser completa, ou seja, retornamos apenas o tipo de retorno
                                                 else Erro " @typechecker: mais argumentos que parametros"  -- argumentos nao podem ser maiores que os parametros
                                               where tksArgs = zipWith (tke tc) lexp pTypes  -- define tksArgs onde faz uma tupla contendo elementos dos argumentos com elementos dos parametros, e manda pro tke para ver se possuem o mesmo tipo
                                                     isThereError l = filter (==False) 
                                                                             (map (\e->(let r2 = e in  
                                                                                            case r2 of
                                                                                              OK _ -> True
                                                                                              Erro _ -> False)) l)
                        OK t -> Erro ("@typechecker: tipo deveria ser funcao em " ++ printTree exp++ " tipo real: " ++ show t)
                        Erro msg -> Erro msg
    -- TODO: o que esta sendo testando abaixo ?                     
    EComp exp1 exp2 -> case (tinf tc exp1, tinf tc exp2) of  -- tenta descobrir o tipo das exps
                        (OK (TFun trExp1 tpsExp1) , OK (TFun trExp2 tpsExp2) )  ->   -- se for OK vao ser tipo funcao
                           if ([trExp2] == tpsExp1)  -- conferimos se o tipo de retorno da 2 expressao eh igual ao tipo de parametro da 1 expressao
                             then OK (TFun trExp1 tpsExp2)  -- se OK entao vai ser uma nova funcao
                             else Erro "erro..."



-- *** nao altere o codigo abaixo ***
combChecks :: TContext -> Exp -> Exp -> Type -> R Type
combChecks tc exp1 exp2 tp = let r = tke tc exp1 tp in
                                       case r of
                                          OK _ -> let r2 = tke tc exp2 tp in
                                                     case r2 of 
                                                         OK _ -> OK tp
                                                         Erro msg -> Erro msg
                                          Erro msg -> Erro msg 
                             

lookup :: TContext -> Ident -> R Type
lookup [] id = Erro ("@typechecker: " ++ printTree id ++ " nao esta no contexto. ")
lookup ((id,value):cs) key
   | id == key = OK value
   | otherwise = lookup cs key


updateTC :: TContext -> Ident -> Type -> R TContext
updateTC [] id tp = OK [(id,tp)]
updateTC ((id,tp):idTps) idN tpN 
  | id == idN = Erro ("@typechecker: identificador" ++ printTree id ++ " nao pode ter mais de um tipo")
  | otherwise = let r = (updateTC idTps idN tpN) in       
                  case r of 
                    OK restOK -> OK ((id,tp) : restOK)    
                    Erro msg -> Erro msg 

getFunctionType :: Function -> Type
getFunctionType (Fun tipoRetorno _ decls _) = TFun tipoRetorno (map (\(Dec tp _ )-> tp) decls)

updatecF :: TContext -> [Function] -> R TContext
updatecF tc [] = OK tc
updatecF tc (f@(Fun _ nomeF _ _):fs) = let r = updateTC tc nomeF (getFunctionType f) in
                                                   case r of 
                                                     OK tcNew -> updatecF tcNew fs
                                                     Erro msg -> Erro msg
                                                     
                                                     
                                                     


