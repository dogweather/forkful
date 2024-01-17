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

# Comparando duas datas em Elm: Por que e como os programadores fazem isso?

## O que e porquê?

Comparar duas datas é uma tarefa comum em programação, especialmente quando trabalhamos com dados relacionados a tempo e agendamento. Essencialmente, estamos comparando duas instâncias de uma data para determinar qual é a mais recente ou se elas são iguais. Isso nos permite tomar decisões lógicas com base nessas datas e é uma habilidade fundamental para muitos programadores.

## Como fazer:

Em Elm, comparar duas datas é bastante simples. Podemos usar a função `Date.compare` que leva duas datas como argumentos e retorna um `Order`. Uma instância de `Order` pode ser `LT` (menos que), `EQ` (igual) ou `GT` (maior que). Aqui está um exemplo de como usá-lo:

```Elm
date1 = Date.fromString "2020-01-01"
date2 = Date.fromString "2020-01-05"
order = Date.compare date1 date2
```

Neste caso, a variável `order` será igual a `LT`, indicando que `date1` é menor que `date2`. Podemos usar isso em uma declaração if para tomar ações diferentes dependendo da data mais recente.

## Profundando:

Comparar duas datas tem sido uma tarefa constante na história da programação. Em linguagens mais antigas, como C ou Java, muitas vezes tínhamos que escrever nosso próprio código para comparar datas, o que poderia levar a erros e ineficiências. Felizmente, em Elm, temos a função `Date.compare` que já lida com toda a lógica de comparação para nós.

No entanto, existem outras maneiras de se comparar datas em Elm. Podemos usar a biblioteca Time Extra que oferece mais recursos relacionados a tempo, incluindo a função `Time.Compare` para comparar datas e até mesmo o `Time.Extra.ExtraOrdinary` para comparar datas com uma precisão maior.

## Veja também:

- Biblioteca Time Extra: https://package.elm-lang.org/packages/elm/time-extra/latest/
- Documentação do Elm sobre comparação de datas: https://package.elm-lang.org/packages/elm/time/latest/Time#compare