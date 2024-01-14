---
title:    "Elm: Comparando duas datas"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por que comparar duas datas em Elm?

Comparar duas datas é uma tarefa comum em muitos projetos de programação. No Elm, essa tarefa pode ser realizada de maneira simples e eficiente, ajudando a garantir a precisão e consistência dos dados em seu aplicativo.

## Como fazer isso em Elm?

Para comparar duas datas em Elm, você precisa primeiro converter ambas as datas para o tipo Date. Isso pode ser feito usando a função `Date.fromString`, que converte uma string em uma data formatada corretamente. Em seguida, use a função `Date.diffInDays` para obter a diferença em dias entre as duas datas.

```
elm
-- Converte duas datas em valores do tipo Date
let date1 = Date.fromString "2020-10-16"
let date2 = Date.fromString "2021-10-16"
-- Obtém a diferença em dias entre as duas datas
let diff = Date.diffInDays date2 date1
```

O valor retornado pela função `Date.diffInDays` será um número do tipo `Result Error Int`, que pode representar um erro (se as datas forem inválidas) ou a diferença em dias entre as duas datas (se forem válidas). Você pode usar a função `Result.withDefault` para lidar com o resultado caso ocorra um erro.

```
elm
-- Lida com o resultado da função Date.diffInDays
let diffInDays = Result.withDefault 0 diff
-- Imprime a diferença em dias
Debug.log "Diferença em dias:" diffInDays
```

## Aprofundando-se na comparação de datas

Ao comparar duas datas em Elm, é importante ter em mente que a precisão pode variar dependendo da unidade usada (ex: dias, horas, minutos). Portanto, é recomendável sempre especificar a unidade desejada ao usar a função `Date.diffIn`. Você também pode usar outras funções, como `Date.diffInSeconds`, `Date.diffInMinutes`, `Date.diffInHours`, para obter a diferença em outras unidades.

Outra consideração importante é a diferença entre datas de diferentes fusos horários. Para garantir que as datas estejam no mesmo fuso horário antes de compará-las, use a função `Date.toUtc` para converter a data para o fuso horário UTC.

## Veja também

- Documentação oficial do Elm para funções de comparação de datas: https://package.elm-lang.org/packages/elm/time/latest/Time
- Tutorial sobre como lidar com datas e tempos em Elm: https://www.codewithhugo.com/working-with-datetime-elm/