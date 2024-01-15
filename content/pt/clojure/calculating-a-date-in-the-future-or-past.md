---
title:                "Calculando uma data no futuro ou no passado"
html_title:           "Clojure: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Você já teve a necessidade de calcular uma data no futuro ou passado? Pode parecer uma tarefa simples, mas pode se tornar complicada quando levamos em conta diferentes calendários, feriados e fuso horário. Felizmente, com a ajuda da linguagem de programação Clojure, podemos facilmente realizar essa tarefa.

## Como fazer

Para calcular uma data no futuro ou passado em Clojure, podemos usar a função `clj-time.core/plus` do pacote `clj-time`. Primeiro, devemos importar esse pacote no nosso código:

```Clojure
(require '[clj-time.core :as time])
```

Em seguida, podemos usar a função `plus` para adicionar uma unidade de tempo à data atual. Por exemplo, para calcular a data de amanhã, podemos usar a seguinte expressão:

```Clojure
(time/plus (time/today) (time/days 1))
```

O resultado será a data de amanhã no mesmo horário da data atual. Podemos também especificar um fuso horário para obter a data correta, por exemplo:

```Clojure
(time/plus (time/today) (time/days 1) (time/time-zone-for-id "America/Sao_Paulo"))
```

Este código irá retornar a data de amanhã no fuso horário de São Paulo. Da mesma forma, podemos usar `time/days` para adicionar dias, `time/hours` para adicionar horas, entre outras unidades de tempo disponíveis. Também podemos usar valores negativos para calcular uma data no passado.

## Aprofundando

A função `plus` também aceita um argumento opcional que especifica o calendário a ser usado. Isso é útil quando precisamos levar em conta feriados ou outros eventos específicos para um determinado calendário. Podemos usar a função `clj-time.coerce/to-date` para converter uma string em uma data no formato desejado. Por exemplo:

```Clojure
(time/plus (time/today-time) (time/days 1) (time/time-zone-for-offset -3) (clj-time.coerce/to-date "2021-08-01T12:00:00Z" "yyyy-MM-dd'T'HH:mm:ss'Z'"))
```

Este código irá retornar a data de amanhã às 12h no fuso horário de Brasília.

## Veja também

- Documentação oficial do pacote `clj-time`: https://github.com/clj-time/clj-time
- Outras funções úteis para trabalhar com datas em Clojure: https://clojureverse.org/t/functions-for-calculating-elapsed-time-using-clj-time/2860