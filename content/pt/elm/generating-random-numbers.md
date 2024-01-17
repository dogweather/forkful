---
title:                "Gerando números aleatórios"
html_title:           "Elm: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Gerar números aleatórios é uma técnica comum usada por programadores para criar valores não previsíveis. Isso pode ser útil em situações como jogos, sorteios ou criptografia.

## Como fazer:

Para gerar um número aleatório no Elm, podemos usar a função `Random.generate`. Veja um exemplo abaixo:

```Elm
import Random exposing (..)

myRandomNumber : Int
myRandomNumber =
  Random.generate (always 10) (int 1 100)
```

Neste exemplo, usamos a função `always` para sempre retornar o número 10 e a função `int` para especificar o intervalo em que queremos gerar o número aleatório, no caso, entre 1 e 100.

O exemplo acima irá gerar um número aleatório entre 1 e 100 toda vez que for executado.

## Profundando:

Gerar números aleatórios pode ser uma tarefa complicada e muitas vezes depende do sistema operacional ou do hardware utilizado. No Elm, temos a garantia de que a sequência de números gerada será a mesma em todos os sistemas e dispositivos.

Alternativamente, é possível gerar números pseudoaleatórios usando a função `Random.initialSeed`. Esta função nos permite especificar uma semente, que será usada para gerar uma sequência de números aleatórios.

Além disso, é importante mencionar que o Elm possui uma biblioteca chamada `elm-random-extra` que oferece funções adicionais para gerar números aleatórios.

## Veja também:

- Documentação oficial do Elm sobre geração de números aleatórios: https://package.elm-lang.org/packages/elm/random/latest/
- Documentação oficial do `elm-random-extra`: https://package.elm-lang.org/packages/elm-community/random-extra/latest/