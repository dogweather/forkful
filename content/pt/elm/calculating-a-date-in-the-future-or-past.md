---
title:                "Cálculo de uma data no futuro ou passado"
html_title:           "Elm: Cálculo de uma data no futuro ou passado"
simple_title:         "Cálculo de uma data no futuro ou passado"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que calcular uma data no futuro ou passado?

Existem muitas situações nas quais precisamos descobrir uma data específica no futuro ou passado. Pode ser para agendar um evento, criar um lembrete ou qualquer outra necessidade relacionada ao tempo. Felizmente, a linguagem Elm oferece uma maneira simples e eficiente de calcular datas.

## Como fazer

Para calcular uma data em Elm, primeiro precisamos importar o módulo `Time`:

```Elm
import Time exposing (..)
```

Em seguida, podemos usar a função `millisToPosix` para converter um número de milissegundos em um valor de data POSIX. Por exemplo, se quisermos calcular uma data 5 dias a partir de hoje, podemos fazer o seguinte:

```Elm
let
    dataFutura = Date.millisToPosix (Time.millisToPosix 5)
in
    "A data daqui a 5 dias é " ++ Date.toString dataFutura
```

Isso nos dará a seguinte saída: "A data daqui a 5 dias é 2021-10-10". Podemos fazer o mesmo para calcular uma data no passado, basta usar um número negativo de milissegundos.

## Mergulho Profundo

A função `millisToPosix` aceita um segundo argumento opcional que nos permite ajustar o fuso horário da data. Por padrão, ele assume o fuso horário padrão UTC, mas podemos especificar um fuso horário específico por meio da função `Time.millisToUtcPosix`. Além disso, o módulo `Time` também oferece outras funções úteis para manipulação de datas, como `calculate` e `since`.

## Veja também
- Documentação oficial sobre o módulo `Time`: https://package.elm-lang.org/packages/elm/time/latest/
- Como manipular e formatar datas em Elm: https://dev.to/mariyadomina/handling-dates-in-elm-3kii
- Exemplos práticos de cálculos de datas em Elm: https://gist.github.com/rtfeldman/b71e0271d419dbdaff904b01bacd2475