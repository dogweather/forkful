---
title:                "Elm: Comparando duas datas"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas é importante em Elm?

Ao trabalhar com programação, frequentemente precisamos comparar duas datas para realizar diferentes tarefas, como ordenar eventos em um calendário ou validar informações de um formulário. Em Elm, comparar duas datas pode ser feito de forma simples e eficiente, por isso é importante entender como isso funciona.

## Como fazer em Elm

Felizmente, comparar datas em Elm é bastante simples. Primeiro, devemos importar o módulo `Date` e criar duas variáveis com as datas que queremos comparar. Em seguida, utilizamos a função `Date.compare` passando as duas datas como argumentos. O resultado será uma `Maybe` que indica se a primeira data é menor, igual ou maior que a segunda.

```elm
-- Importar o módulo Date
import Date exposing (..)

-- Criar as duas datas a serem comparadas
data1 = Date.fromCalendarDate 2021 5 10
data2 = Date.fromCalendarDate 2021 5 15

-- Comparar as datas e obter o resultado
resultado = Date.compare data1 data2
```

O resultado será `Just LT`, indicando que a primeira data (data1) é menor que a segunda (data2). Caso o resultado seja `Just EQ`, significa que as datas são iguais, e se for `Just GT`, a primeira data é maior que a segunda.

Caso as datas não possam ser comparadas (por exemplo, uma delas é inválida), o resultado será `Nothing`. Portanto, é importante sempre tratar esse retorno para evitar erros em nosso código.

## Mergulho profundo

Em Elm, as datas são representadas como números de milissegundos desde o Unix Epoch (1 de janeiro de 1970). Isso significa que podemos também trabalhar diretamente com esses números para comparar datas em formato de milissegundos.

Além disso, existem outras funções úteis no módulo `Date` que podem nos auxiliar no processo de comparação, como `toTime` e `fromTime`. Vale a pena explorar essas funções e aprender mais sobre a manipulação de datas em Elm.

## Veja também

- Documentação do módulo Date do Elm: https://package.elm-lang.org/packages/elm/core/latest/Date
- Tutorial sobre comparação de datas em Elm: https://medium.com/@gumsoul/comparing-dates-in-elm-b1622265baf0