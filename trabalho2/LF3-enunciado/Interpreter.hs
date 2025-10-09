module Interpreter where

import AbsLF
import AbsLFAux  -- TODO: leia agora o conteudo desse arquivo (AbsLFAux.hs) e explique por que refatoramos assim 
-- manutenabilidade, organizacao, etc -> alem das boas praticas em separar arquivos com propostas diferentes (AbsLF define a nossa AST e os getters buscam informacoes)
-- os getters ficam em outro arquivo porque a AbsLF eh gerada automaticamente quando usamos BNFC, se colocarmos um codigo manual em um arquivo gerado automaticamente
-- toda vez que usarmos BNFC o nosso codigo inserido manualmente sera apagado
import Prelude hiding (lookup)


executeP :: Program -> Valor

executeP (Prog fs) =  eval (updatecF [] fs) (expMain fs)
    where expMain (f:xs) 
              | (getName f == (Ident "main")) =  getExp f
              | otherwise = expMain xs                                            

type RContext = [(Ident,Valor)]
   
eval :: RContext -> Exp -> Valor
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
    EIf exp expT expE -> if ( i (eval context exp) /= 0) 
                          then eval context expT
                          else eval context expE
    -- TODO: na linha abaixo, retorne um ValorFun contendo o lambda e saiba explicar a razao         
    -- O objetivo aqui nao é executar lambda, mas sim envelopar ela em um Valor para ser executada mais tarde com ECall                   
    lambda@(ELambda params exp) -> ValorFun lambda
    -- TODO: em EComp abaixo, troque undefined (2 ocorrencias) pela construcao apropriada                           
    EComp exp1 exp2 ->  let (ValorFun exp1') = eval context exp1
                            (ValorFun exp2') = eval context exp2 in 
                          ValorFun(ELambda (getParamsTypesL exp2') 
                                           (ECall exp1' [ECall exp2' (getParamsExpL exp2')]))             
    {- TODO: em ECall abaixo, troque undefined (3 ocorrencias) pela construcao apropriada.                           
       Dica: estude o codigo, buscando entender tambem as definicoes locais -}
    ECall exp lexp ->  if (length lexp < length parameters) 
                         then ValorFun (ELambda params' exp') -- TODO: que caso eh esse ?   Aplicacao parcial
                         else eval (paramBindings ++ contextFunctions) exp' -- TODO: que caso eh esse ?   Aplicacao completa
                        where (ValorFun lambda) = eval context exp
                              parameters = getParamsL lambda
                              paramBindings = zip parameters (map (eval context) lexp)
                              params' = drop (length lexp) (getParamsTypesL lambda)
                              exp' = subst paramBindings (getExpL lambda) 
                              contextFunctions = filter (\(i,v) -> case v of 
                                                                         ValorFun _ -> True 
                                                                         _ -> False
                                                           ) 
                                                          context


-- a função "subst" gera uma nova expressao a partir dos bindings em RContext
subst :: RContext -> Exp -> Exp 
subst rc exp  = case exp of  
    EVar id        -> bind id rc -- TODO: por que eh implementado assim ?  
    -- esse eh o caso base da substituicao de variavel, quando encontra uma EVar ela para de descer na arvore, e na hora de substituir ele delega a tarefa para a
    -- funcao bind que vai pegar o identificador e o contexto de substituicao para realizar a substituicao e retornar a expressao

    -- TODO: explique a implementacao da linha abaixo
    lambda@(ELambda paramsTypes exp) -> ELambda paramsTypes (subst (rc `diff` (getParamsL lambda)) exp)
    -- sombreamento de variavel, ela garante que os parametros de uma funcao nao sejam substituidos por uma variavel de fora com o mesmo nome 
    -- diff remove da lista de substituicao rc as variaveis que foram "sombreadas" pelos parametros da lambda

    ECall exp lexp -> ECall (subst rc exp ) (map (subst rc) lexp)
    EAdd exp0 exp  -> EAdd (subst rc exp0 ) (subst rc exp )
    -- TODO: nos casos abaixo, troque cada undefined pela construcao apropriada
    EComp exp1 exp2 -> EComp (subst rc exp1) (subst rc exp2)
    EIf expC expT expE -> EIf (subst rc expC) (subst rc expT) (subst rc expE)
    ECon exp0 exp  -> ECon (subst rc exp0) (subst rc exp)
    ESub exp0 exp  -> ESub (subst rc exp0) (subst rc exp)
    EMul exp0 exp  -> EMul (subst rc exp0) (subst rc exp)
    EDiv exp0 exp  -> EDiv (subst rc exp0) (subst rc exp)
    EOr  exp0 exp  -> EOr (subst rc exp0) (subst rc exp)
    EAnd exp0 exp  -> EAnd (subst rc exp0) (subst rc exp)
    ENot exp       -> ENot (subst rc exp) 
    _ -> exp   -- TODO: quais sao esses casos e por que sao implementados assim ?       serao os literais, caso baso que nao faz nada e retorna a expressao inalterada         

{- TODO: 
  sobre a implementacao finalizada de subst:
  1) qual eh o caso base?  EVar que realiza a substituicao e _ que lida com literais e nao faz nada

  2) como descrever o numero de casos recursivos? depende (in)diretamente de algo? Depende diretamente da quantidade de nós na arvore que nao sejam folhas

  3) qual a finalidade dos casos recursivos? A finalidade eh percorrer a AST e desmontando os nós para realizar a substituicao e montar novamente com as variaveis substituidas

  4) por que a linha 64 eh diferente dos outros casos recursivos? Porque ELambda eh o unico que introduz um novo escopo de variaveis, ela precisa do diff para tratar o sombreamento
  -- de variaveis, que é algo que as outras nao fazem

  5) numa especificacao textual intuitiva e concisa (semelhante ao comentario na linha 59),
     qual a linha mais importante entre 62-77 ?
     -- A linha mais importante é a lambda que  implementa a semantica de escopo da linguagem, que é a parte fundamental para o funcionamento das funções

  6) Ha semelhanca de implementacao em relacao ao Optimizer.hs? Qual(is)?    sim, subst e optimizeE usam recursao com "case exp of" para percorrer todos os tipos de expressao
-}

-- a função "diff" faz a diferença, tirando de RContext os mapeamentos envolvendo [Ident].
diff :: RContext -> [Ident] -> RContext
rc `diff` [] = rc
[] `diff` _ = [] 
((k,v):kvs) `diff` (id:ids) 
    | k == id =  kvs `diff` ids
    | otherwise = (k,v) : ( kvs `diff` (id:ids))

-- a função bind retorna uma expressao contendo o valor do id no RContext, ou o proprio id. 
-- TODO: por que nao usamos o lookup no lugar de bind ?  lookup tem que retornar um Valor, ou seja, se nao achar a variavel ele quebra, enquanto o bind
-- seria mais seguro porque se ele nao achar a variavel ele retorna a variavel original, o que seria o comportamento correto para uma substituicao
bind :: Ident -> RContext -> Exp
bind id [] = EVar id  -- retorna o proprio id se ele nao esta ligado em RContext
bind id ((k,v):kvs)
    | k == id = wrapValueExpression v 
    | otherwise = bind id kvs 

-- "wrapValueExpression" empacota um valor em uma expressao 
wrapValueExpression :: Valor -> Exp 
wrapValueExpression (ValorInt i) = EInt i
wrapValueExpression (ValorStr s) = EStr s
wrapValueExpression (ValorBool True) = ETrue
wrapValueExpression (ValorBool False) = EFalse
wrapValueExpression (ValorFun exp) = exp 


data Valor = ValorInt {
               i :: Integer         
             }
            | 
             ValorFun {
               f :: Exp   --f :: Function  **NOVO TODO: Por que mudou ?   para guardar o valor Elambda diretamente
             }   
            | 
             ValorStr {
               s :: String
             } 
            | ValorBool {
               b :: Bool
             }

instance Show Valor where
  show (ValorBool b) = show b
  show (ValorInt i) = show i
  show (ValorStr s) = s
  show (ValorFun f) = show f  -- TODO: por que essa linha funciona ?  BNFC cria uma instancia de show que sabe como transformar empressao em string legivel
  

lookup :: RContext -> Ident -> Valor
lookup ((i,v):cs) s
   | i == s = v
   | otherwise = lookup cs s

update :: RContext -> Ident -> Valor -> RContext
update [] s v = [(s,v)]
update ((i,v):cs) s nv
  | i == s = (i,nv):cs
  | otherwise = (i,v) : update cs s nv


-- NOVO: TODO: explique a mudanca em updatecF
updatecF :: RContext -> [Function] -> RContext
updatecF c [] = c
updatecF c (f:fs) = updatecF (update c (getName f)    
                                       (ValorFun (ELambda (getParams f) (getExp f)))) 
                              fs
-- updatecF c (f:fs) = updatecF (update c (getName f) (ValorFun f)) fs

-- Antes o ValorFun esperava um tipo Function. Na nova versao o tipo Function tem seu corpo e parametros extraidos e envelopados por uma ELambda, criando um noh com elas que
-- vai ser do tipo exp, que eh o tipo que o ValorFun espera agora
