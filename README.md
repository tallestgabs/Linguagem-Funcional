# Trabalho 1
* Entrega: apresentação oral na aula do dia 19/09.

* Observações:
 --> Para as Partes 1 e 2 abaixo, utilize como base o arquivo  trabalho1.zip, que é fornecido abaixo.   Ao descomprimir esse arquivo, haverá 2 diretórios:  LF1-Enunciado e LF2-Enunciado,  correspondentes às Partes 1 e 2 abaixo.
  --> O trabalho pode ser individual ou em dupla. Se for em dupla, ambos membros devem dominar o conteúdo apresentado como resposta do trabalho.  

* Parte 1  (5,0 pontos): LI2 -> LF1. Diretório LF1-Enunciado. 
 Evoluir a LI2 para se tornar funcional (LF1), fazendo as adaptações necessárias no interpretador. A sintaxe abstrata da LF1 está no arquivo AbsLF.hs. O trabalho envolve entender tal sintaxe e alterar o Interpreter.hs, conforme instruções TODO e dicas @dica. Há programas de teste no diretório examples.

* Parte 2 (5,0 pontos) LF1 -> LF2. Diretório LF2-Enunciado.
Evoluir a LF1 para adicionar checagem de tipos (em parte semelhante à LI2Tipada)
 e otimização estática. Estude a sintaxe abstrata da LF2, que está no arquivo AbsLF.hs.   
São fornecidos arquivos iniciais com tarefas e dicas específicas para o 
checador de tipos e o otimizador. Estude os arquivos Optimizer.hs e Typechecer.hs, 
completando as tarefas (indicadas por TODO e veja as dicas @dica). Além disso, 
você deve fornecer o arquivo Interpreter.hs com a semântica da execução.  A 
recomendação é reaproveitar o Interpreter.hs da LF1 redefinindo as funções indicadas no 
início do arquivo que extraem informação de um valor do tipo Function (getName, getParams, getExp).
# Trabalho 2
* (LF3) ->
*Data de apresentação/arguição em sala de aula: 10/10*

* O trabalho pode ser individual ou em dupla (preferencialmente)

* Evoluir a LF2 para promover funções como valores. Isso significa prover as seguintes 
funcionalidades:

1. expressão lambda
2. aplicação parcial
3. composição de funções 
* No arquivo comprimido, é fornecida a sintaxe concreta (LF3.cf), a partir da qual a sintaxe abstrata, o lexer e o parser podem ser gerados pelo BNFC. Também são fornecidos arquivos iniciais para o type checker (Typechecer.hs), otimizador (Optimizer.hs),  e semântica de execução (Interpreter.hs). Por fim, são fornecidos o driver do interpretador  (Interpret.hs), um arquivo auxiliar de sintexe abstrata (AbsLFAux.hs), e o  diretório "examples" com programas de teste escritos na LF3.

### Tarefas

* Estude o arquivo LF3.cf, localizando as mudanças (procure por "NOVO"). O arquivo poderá ser consultado por ocasião da criação de novos programas exemplos.
Gere a sintaxe abstrata, o lexer, e o parser a partir do BNFC.    
* Estude a sintaxe abstrata (AbsLF.hs), observando o que mudou em relação à LF2.
Realize as tarefas TODO nos arquivos Typechecer.hs, Optimizer.hs, e Interpreter.hs
Favor nao apagar os comentários TODO, pois serao usados para guiar a arguicao.
Execute os testes no diretório "examples" para testar o interpretador.
Crie novos programas de teste para testar as funcionalidades novas da LF3. 
*Bom trabalho!*

# Trabalho 3

Apresentação: 19/11 e 21/11

* Parte 1- (5,0 pontos)- Otimização em tempo de execução com memoização

### Descrição:

Estenda semântica operacional da LF2 com memoização de chamadas de funções nomeadas.
No arquivo Interpreter.hs, quando uma função nomeada (não é uma expressão lambda) for chamada
com determinados argumentos, checa-se se a chamada já foi feita. Caso sim, retorna-se o valor;
caso contrário, evalia-se a chamada e retorna-se um novo contexto com esse novo resultado
para eventual uso futuro, a fim de evitar recomputação.

Teste num programa de Fibonacci com e sem memoização. Teste em C e em Python
ou qualquer outra linguagem de preferência e veja as limitações.

===============================================================

* Parte 2- (5,0 pontos)- Refatoração Monads/SYB

Descrição: Utilize Monads e SYB para refatorar o interpretador da LF2. Algumas possibilidades
são no otimizador (Optimizer.hs), checador de tipos (Typechecer.hs), e semântica operacional
(Interpreter.hs).
