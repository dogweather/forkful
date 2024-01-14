---
title:    "Elm: Comparando duas datas"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas em Elm?

Comparar datas é uma tarefa comum em muitos projetos de programação. Em Elm, essa tarefa pode ser facilmente realizada com algumas funções nativas. Neste artigo, vamos explorar como comparar duas datas em Elm e como essa habilidade pode ser útil para criar aplicações mais robustas.

## Como fazer:

Para comparar duas datas em Elm, vamos utilizar a função `Date.compare`, que recebe duas datas como argumento e retorna um `Date.Comparison`. Este tipo pode ter três valores possíveis: `LT` (menor que), `EQ` (igual a) ou `GT` (maior que).

```
Elm Date.compare example:

import Date exposing (Date)
import Date exposing (Month(..))

januaryFirst : Date
januaryFirst = Date.fromYearMonthDay2021 1 1 0 0 0

marchTwelfth : Date
marchTwelfth = Date.fromYearMonthDay2021 3 12 0 0 0

comparison : Date.Comparison
comparison = Date.compare januaryFirst marchTwelfth

-- Output: Date.GT
```

Neste exemplo, criamos duas datas diferentes e as comparamos utilizando a função `Date.compare`. No final, a variável `comparison` recebe o valor `Date.GT`, indicando que a segunda data é maior que a primeira.

Outra função útil para realizar comparações é a `Date.isSame`, que recebe duas datas e retorna um `Bool`, indicando se elas são iguais ou não. Além disso, podemos utilizar os operadores `>`, `<`, `==` para comparar diretamente se uma data é maior, menor ou igual a outra.

```
Elm Date.isSame example:

import Date exposing (Date)
import Date exposing (Month(..))

januaryFirst : Date
januaryFirst = Date.fromYearMonthDay2021 1 1 0 0 0

marchTwelfth : Date
marchTwelfth = Date.fromYearMonthDay2021 3 12 0 0 0

isSame : Bool
isSame = Date.isSame januaryFirst marchTwelfth

-- Output: False
```

Este exemplo utiliza a função `Date.isSame` para comparar as mesmas datas do exemplo anterior e retorna `False`, pois elas são diferentes.

## Profundidade da função de comparação de datas:

A função `Date.compare` é mais profunda do que parece à primeira vista. Além de comparar apenas as datas, essas funções também levam em conta o horário, fuso horário e até mesmo o formato em que as datas estão armazenadas. Isso significa que, mesmo que duas datas sejam tecnicamente iguais, se elas tiverem diferenças em algum desses aspectos, a comparação pode retornar um valor diferente de `EQ`.

Por exemplo, se uma data estiver no formato de 12h e a outra no formato de 24h, elas serão consideradas diferentes pela função `Date.compare`, mesmo que sejam essencialmente a mesma data.

É importante ter em mente esses detalhes ao utilizar as funções de comparação de datas em seus projetos Elm.

## Veja também:

- Documentação sobre como comparar datas em Elm: https://package.elm-lang.org/packages/time/1.0.0/Date#comparison
- Artigo sobre como realizar cálculos com datas em Elm: https://medium.com/@robertsimoes/using-date-math-in-elm-9020403f9d30
- Vídeo tutorial sobre como trabalhar com datas em Elm: https://www.youtube.com/watch?v=M7xLQ50vnos