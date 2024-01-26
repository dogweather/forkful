---
title:                "Gerando números aleatórios"
date:                  2024-01-20T17:48:53.087283-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gerando números aleatórios"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Gerar números aleatórios significa criar números que não podem ser previstos logicamente. Programadores fazem isso para funcionalidades como jogos, simulações ou para gerar dados aleatórios em testes.

## Como Fazer:
```Elm
import Random

-- Gerando um número aleatório entre 1 e 100
randomNumber : Random.Generator Int
randomNumber = Random.int 1 100

-- Para utilizar o gerador, precisamos de uma semente e executar em um programa:
main =
    let
        (number, _) = Random.step randomNumber (Random.initialSeed 42)
    in
    -- Isto irá mostrar um número aleatório no console.
    Html.text (String.fromInt number)
```

## Mergulho Profundo
Historicamente, a geração de números aleatórios nos computadores sempre foi um desafio, pois a lógica computacional é determinística. Alternativas para Elm incluem geradores de números pseudoaleatórios em diferentes linguagens, cada um com suas implementações e usos variados. Elm usa um sistema funcional que separa a geração de números do estado aleatório (a semente), o que facilita a previsibilidade em testes e evita efeitos colaterais.

## Veja Também
- Documentação oficial Elm sobre números aleatórios: https://package.elm-lang.org/packages/elm/random/latest/
- Um guia para testes em Elm com números aleatórios: https://elmprogramming.com/randomness.html
- Artigo sobre a história da geração de números aleatórios em computadores: https://en.wikipedia.org/wiki/Random_number_generation
