---
title:                "Converter uma data em uma string"
html_title:           "Elm: Converter uma data em uma string"
simple_title:         "Converter uma data em uma string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O que & Por que?

Converter uma data em uma string em Elm significa transformar uma data em um formato legível para humanos, como "12 de abril de 2021". Programadores fazem isso para facilitar a exibição de datas em aplicativos e sites.

## Como fazer:

Em Elm, a data é representada pelo tipo `Date` e pode ser convertida em uma string através da função `toString`. Veja um exemplo:

```Elm
myDate : Date
myDate = Date.fromFields 2021 4 12

toString myDate
-- "12 de abril de 2021"
```

## Profundidade:

O formato de data mais comum em Elm é o ISO-8601, que segue o padrão `YYYY-MM-DD`. No entanto, também é possível usar outros formatos, como `dd/MM/yyyy`. Uma alternativa para converter datas em strings é usar a biblioteca `elm/time`.

## Veja também:

- Documentação oficial do Elm sobre `Date`: https://package.elm-lang.org/packages/elm/time/latest/Time-Date
- Mais informações sobre ISO-8601: https://pt.wikipedia.org/wiki/ISO_8601