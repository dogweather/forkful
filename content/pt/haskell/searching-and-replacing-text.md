---
title:                "Pesquisando e substituindo texto"
html_title:           "Bash: Pesquisando e substituindo texto"
simple_title:         "Pesquisando e substituindo texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Substituição de texto na Haskell: Mais Simples do Que Você Imagina

## O que e Porquê?

A busca e substituição de texto envolve identificar todas as ocorrências de uma string específica (a ser buscada) e substituí-la por outra. Programadores fazem isso para mapear variáveis no código, atualizar funções antigas ou melhorar a legibilidade do código programado.

## Como Fazer:

Em Haskell, a função `substitute` da biblioteca Data.String.Utils é frequentemente usada.

Instale com o seguinte comando:
```Haskell
cabal install MissingH
```
Aqui está um exemplo de código:
```Haskell
import Data.String.Utils

main = print $ substitute "world" "Haskell" "Hello, world!"
```
A saída será:
```Haskell
"Hello, Haskell!"
```
Neste exemplo, o programa procura pela string "world" na frase "Hello, world!" e a substitui por "Haskell".

## Mergulho Profundo

Haskell é uma linguagem de programação puramente funcional desenvolvida no final dos anos 80. Uma das alternativas para a função `substitute` é a função `replace`, da biblioteca `Text`.

Vale lembrar que a função `substitute` tem complexidade O(n). A complexidade também pode ser afetada dependendo do tamanho do texto a ser pesquisado e substituído.

## Veja Também:

Para mais informações e assuntos relacionados, você pode visitar os seguintes links:

1. [Haskell: Guia Básico para Iniciantes](https://learnxinyminutes.com/docs/pt-br/haskell-pt/)
   
2. [Documentação Oficial da Linguagem Haskell](https://www.haskell.org/documentation/)

3. [Data.String.Utils](http://hackage.haskell.org/package/MissingH-1.2.1.0/docs/Data-String-Utils.html)
   
4. [Text](http://hackage.haskell.org/package/text-1.2.3.1/docs/Data-Text.html)

Dê uma olhada nos recursos acima para aprender mais sobre Haskell e sua aplicação na busca e substituição de texto.