---
title:                "Comparando duas datas"
date:                  2024-01-20T17:32:41.189069-07:00
model:                 gpt-4-1106-preview
simple_title:         "Comparando duas datas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?

Comparar duas datas significa verificar se são iguais, qual vem antes ou depois da outra. Programadores fazem isso para organizar eventos, filtrar prazos, programar lembretes e mais.

## Como Fazer:

Para comparar datas em Elm, você pode utilizar a biblioteca `elm/time`. Aqui estão alguns exemplos práticos:

```Elm
import Time exposing (Posix)
import Time.Extra exposing (compare)

compareDates : Posix -> Posix -> Order
compareDates date1 date2 =
    compare date1 date2

-- Exemplos:
date1 : Posix
date1 = 
    Time.millisToPosix 1500000000000

date2 : Posix
date2 = 
    Time.millisToPosix 1600000000000

-- Quando você compara...
exampleComparison : Order
exampleComparison = compareDates date1 date2

-- ...o resultado que você obtém é LT, GT, ou EQ.
-- LT significa que a primeira data é anterior à segunda.
-- GT significa que a primeira data é posterior à segunda.
-- EQ significa que as datas são iguais.
```

## Mergulho Profundo:

Comparar datas não é algo novo. Era vital para organizar registros históricos, mesmo antes dos computadores. No Elm, a comparação é feita sobre o tipo `Posix`, que representa um ponto no tempo em milissegundos desde a época Unix (1 de janeiro de 1970). Alternativas incluem uso de bibliotecas de terceiros que fornecem funções adicionais, como fusos horários e formatação. Na implementação, Elm usa funções nativas do JavaScript através de ports ou encodings internos para trabalhar com datas e horas eficientemente.

## Veja Também:

- Documentação oficial do `elm/time`: https://package.elm-lang.org/packages/elm/time/latest/
- Documentação do `Time.Extra` na biblioteca `justinmimbs/time-extra`: https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/
- Um excelente artigo sobre manipulação de data e hora em Elm: https://elmprogramming.com/dates.html