module Optimizer where

import AbsLF
import Interpreter

optimizeP :: Program -> Program
optimizeP (Prog fs) = Prog (map optimizeF fs)
       
optimizeF :: Function -> Function
optimizeF (Fun tR id decls exp) = Fun tR id decls (optimizeE exp)

optimizeE :: Exp -> Exp
optimizeE exp  = case exp of
                      EStr str -> EStr str
                      ETrue    -> ETrue
                      EFalse   -> EFalse
                      EInt n   -> EInt n
                      EVar id  -> EVar id
                      ENot exp -> let optExp  = optimizeE  exp
                                      optENot = ENot optExp in
                                      if (isLiteral optExp) 
                                        then  wrapValueExpression (eval [] optENot )
                                        else  optENot
                      ECon exp0 exp -> let optExp0 = optimizeE  exp0 
                                           optExp  = optimizeE  exp   
                                           optECon = ECon optExp0 optExp in 
                                           if ((isLiteral optExp0) && (isLiteral optExp))
                                             then wrapValueExpression (eval [] optECon)
                                             else optECon
                      EAdd exp0 exp -> let optExp0 = optimizeE exp0
                                           optExp  = optimizeE exp  
                                           optEAdd = EAdd optExp0 optExp in 
                                             if ((isLiteral optExp0) && (isLiteral optExp)) 
                                               then wrapValueExpression (eval [] optEAdd)
                                               else optEAdd
-- TODO: substitua "undefined" abaixo pela otimização correspondente ao tipo de expressão.
-- @dica: estude a implementação fornecida da otimização das expressões anteriores
                      ESub exp0 exp ->  let optExp0 = optimizeE exp0
                                            optExp = optimizeE exp
                                            optESub = ESub optExp0 optExp in
                                                if((isLiteral optExp0) && (isLiteral optExp))
                                                  then wrapValueExpression (eval [] optESub)
                                                  else optESub

                      EMul exp0 exp ->  let optExp0 = optimizeE exp0 
                                            optExp = optimizeE exp 
                                            optEMul = EMul optExp0 optExp in 
                                              if((isLiteral optExp0) && (isLiteral optExp))
                                                then wrapValueExpression (eval [] optEMul)
                                                else optEMul 

                      EDiv exp0 exp -> let  optExp0 = optimizeE exp0 
                                            optExp = optimizeE exp 
                                            optEDiv = EDiv optExp0 optExp in 
                                              if((isLiteral optExp0) && (isLiteral optExp))
                                                then wrapValueExpression (eval [] optEDiv)
                                                else optEDiv

                      EOr  exp0 exp -> let  optExp0 = optimizeE exp0 
                                            optExp = optimizeE exp 
                                            optEOr = EOr optExp0 optExp in 
                                              if((isLiteral optExp0) && (isLiteral optExp))
                                                then wrapValueExpression (eval [] optEOr)
                                                else optEOr 

                      EAnd exp0 exp -> let  optExp0 = optimizeE exp0 
                                            optExp = optimizeE exp 
                                            optEAnd = EAnd optExp0 optExp in 
                                              if((isLiteral optExp0) && (isLiteral optExp))
                                                then wrapValueExpression (eval [] optEAnd)
                                                else optEAnd 
-- TODO: saiba explicar o motivo da otimização abaixo 					  
                      ECall id lexp   -> ECall id (map (\expr ->  optimizeE expr) lexp)
                      -- vai car no pattern matching do ECall e vai desmontar o identificador e a lista de expressao
                      -- o retorno continuara sendo uma chamada com o mesmo nome, porem cada elemento da lista sera otimizada atraves do optimizeE, o que no fim retornara 
                      -- a lista de parametros otimizada
                       
-- TODO: crie um programa exemplo em que a otimização abaixo seja usada
                      EIf exp expT expE -> let optExp  = optimizeE exp 
                                               optThen = optimizeE expT
                                               optElse = optimizeE expE 
                                               optEIf  = EIf optExp optThen optElse in 
                                                 case optExp of
                                                   EInt vExpIf -> if (vExpIf == 0)
                                                                     then optElse
                                                                     else optThen
                                                   _              -> optEIf

                        
isLiteral :: Exp -> Bool
isLiteral exp = case exp of
                        EStr  _        -> True
                        ETrue          -> True
                        EFalse         -> True
                        EInt  _        -> True
                        _              -> False

wrapValueExpression :: Valor -> Exp 
wrapValueExpression (ValorInt i) = EInt i
wrapValueExpression (ValorStr s) = EStr s
wrapValueExpression (ValorBool True) = ETrue
wrapValueExpression (ValorBool False) = EFalse



