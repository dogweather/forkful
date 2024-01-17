---
title:                "Obtendo a data atual."
html_title:           "Elm: Obtendo a data atual."
simple_title:         "Obtendo a data atual."
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O que é & por quê?

Obter a data atual é um processo simples que permite aos programadores obter a data mais recente no formato desejado. Isso é útil para vários propósitos, como registrar a data em que um determinado evento ocorreu ou manter o controle de transações em um sistema.

## Como fazer:

Obter a data atual no Elm é bastante fácil. Com o uso da função `toIsoString` do pacote `Time`, podemos obter a data atual no formato ISO padrão. Veja um exemplo abaixo:

```elm
import Time exposing (toIsoString)

currentDate : String
currentDate =
  toIsoString Time.utc
```

O código acima irá retornar uma string contendo a data atual no formato "YYYY-MM-DD". Você também pode ajustar o fuso horário passando uma configuração diferente para a função `toIsoString`. Por exemplo:

```elm
toIsoString (Time.zone 3) -- para obter a data com +3 horas do fuso horário UTC
```

## Mergulho Profundo:

Obter a data atual é uma tarefa tão comum que é suportada por várias linguagens de programação e bibliotecas. No entanto, o método para fazer isso pode variar de acordo com a linguagem que você está usando. Em Elm, a biblioteca `Time` fornece funções convenientes para trabalhar com datas e horários, incluindo a obtenção da data atual.

Caso você esteja trabalhando com JavaScript embarcado no Elm, também é possível obter a data atual usando a biblioteca `Date` do JavaScript. No entanto, isso é considerado uma prática não recomendada, pois pode causar problemas de compatibilidade e desempenho.

## Veja também:

- Documentação oficial da biblioteca `Time` do Elm: [https://package.elm-lang.org/packages/elm/time/latest/Time#toIsoString](https://package.elm-lang.org/packages/elm/time/latest/Time#toIsoString)
- Documentação oficial da biblioteca `Date` do JavaScript para Elm: [https://package.elm-lang.org/packages/elm/core/latest/Date](https://package.elm-lang.org/packages/elm/core/latest/Date)