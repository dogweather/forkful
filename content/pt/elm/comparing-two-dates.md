---
title:                "Comparando duas datas"
html_title:           "Elm: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que

Se você já precisou comparar duas datas em sua programação, sabe como pode ser uma tarefa complexa. Com o Elm, essa tarefa se torna muito mais simples e eficiente. Neste artigo, vamos explorar como comparar duas datas utilizando a linguagem de programação Elm.

## Como Fazer

Começando com dois exemplos de datas:

```elm
date1 = Date.fromParts 2020 04 15
date2 = Date.fromParts 2020 03 30
```

Podemos compará-las utilizando a função `compare` da biblioteca `Date`:

```elm
Date.compare date1 date2
-- Output: Greater
```

O resultado dessa comparação é um tipo de dados `Order`. Ele pode assumir três valores: `Less`, `Equal` e `Greater`, representando respectivamente, menor, igual e maior.

Para uma comparação mais específica, podemos utilizar a função `compareValue` da biblioteca `Date`:

```elm
Date.compareValue date1 date2
-- Output: 16
```

O resultado é um número inteiro que representa a diferença em milissegundos entre as duas datas. Neste caso, a diferença é de 16 dias.

## Deep Dive

Para entendermos melhor como a comparação de datas funciona em Elm, vamos dar uma olhada no código fonte da função `compare`:

```elm
compare : Date -> Date -> Order
compare =
    Date.compareValue >> compareInt
```

Esta função simplesmente chama `Date.compareValue` e passa o resultado para `compareInt`, que converte o número em um valor do tipo `Order`.

É importante lembrar que a comparação de datas em Elm leva em consideração o fuso horário. Se você precisar comparar datas em um fuso horário específico, é preciso utilizar as funções `toIsoStringInTimezone` e `fromIsoStringInTimezone` e especificar o fuso horário desejado.

## Veja Também

- [Documentação oficial do pacote Date no Elm](https://package.elm-lang.org/packages/elm/core/latest/Date)
- [Artigo sobre como lidar com datas em Elm](https://medium.com/frontend-digest/how-to-handle-dates-in-elm-the-right-way-dfb71c658e76)