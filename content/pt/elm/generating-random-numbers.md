---
title:                "Gerando números aleatórios"
html_title:           "C: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Que & Por Que?

Gerar números aleatórios é o processo de criar números que não têm padrão previsível. Programadores fazem isso para muitos fins, como simulações, jogos e testes de programas, onde a aleatoriedade é necessária.

## Como Fazer:

A geração de números aleatórios no Elm é realizada usando o módulo `Random`. Aqui está um exemplo de como gerar um número inteiro aleatório entre 1 e 100.
```Elm
import Random exposing (Generator, int)

randomInt : Generator Int
randomInt =
    int 1 100
```
Depois de definir o gerador, podemos usá-lo dentro de uma função para gerar um valor aleatório.
```Elm
import Random exposing (Generator, int, generate)

randomInt : Generator Int
randomInt =
    int 1 100

generateRandomInt : Generator Int -> Cmd msg
generateRandomInt gen =
    generate always gen
```
## Deep Dive

Historicamente, a geração de números aleatórios tem sido uma parte importante da programação desde o início dos computadores. No entanto, é importante lembrar que a geração de números aleatórios em computadores é, na verdade, pseudoaleatória. Isso significa que, dada a mesma "semente", a sequência de números gerados será a mesma.

Alternativas para a geração de números aleatórios no Elm incluem o uso de diferentes distribuições, como uniforme ou normal, dependendo do uso pretendido. Além disso, a função `float` pode ser usada para gerar números aleatórios de ponto flutuante.

Os detalhes de implementação para a geração de números aleatórios no Elm estão escondidos, como deveriam estar em uma linguagem de programação de alto nível. No entanto, é provavelmente baseado em algum tipo de gerador de número pseudoaleatório, semelhante a outras linguagens de programação modernas.

## Veja Também

Módulo `Random`: [Elm Documentation](https://package.elm-lang.org/packages/elm/core/latest/Random)

Gerador Pseudoaleatório: [Wikipedia](https://pt.wikipedia.org/wiki/Gerador_de_números_pseudoaleatórios)

Distribuições de Probabilidade: [Wikipedia](https://pt.wikipedia.org/wiki/Distribuição_de_probabilidade)