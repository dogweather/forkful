---
title:                "Clojure: Comparando duas datas"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas pode ser útil na programação

Comparar datas é uma tarefa comum na programação, especialmente em casos em que precisamos verificar se uma data é anterior, posterior ou igual a outra. Isso pode ser útil em diversos contextos, como por exemplo em aplicações bancárias, de agendamento de compromissos, ou até mesmo em jogos que envolvam tempo.

## Como comparar duas datas em Clojure

Em Clojure, podemos facilmente comparar datas utilizando a função `compare`. Esta função recebe duas datas como parâmetros e retorna um inteiro que representa a ordem entre elas. Se o resultado for menor que 0, significa que a primeira data é anterior à segunda. Se for igual a 0, as datas são iguais. E se for maior que 0, a primeira data é posterior à segunda.

```Clojure
(let [data1 (java.util.Date.) ; data atual
      data2 (java.util.Date. 2021 8 15)] ; data específica: 15 de agosto de 2021
  (compare data1 data2)) ; retorna -1 pois data1 é anterior à data2
```

```Clojure
(let [data1 (java.util.Date. 2021 9 20) ; data específica: 20 de setembro de 2021
      data2 (java.util.Date. 2021 9 20)] ; data específica: 20 de setembro de 2021
  (compare data1 data2)) ; retorna 0 pois as duas datas são iguais
```

A função `compare` também funciona com instâncias da classe `LocalDateTime` do pacote `java.time`.

```Clojure
(let [data1 (java.time.LocalDateTime/now) ; data e hora atual
      data2 (java.time.LocalDateTime/of 2021 10 31 15 30)] ; data e hora específicas: 31 de outubro de 2021 às 15:30
  (compare data1 data2)) ; retorna 1 pois data1 é posterior à data2
```

## Mergulhando mais fundo nas comparações de datas

Além da função `compare`, Clojure também possui as funções `before?`, `after?` e `=” que facilitam ainda mais a comparação de datas. A função `before?` retorna `true` se a primeira data for anterior à segunda, `false` caso contrário. A função `after?` retorna `true` se a primeira data for posterior à segunda, `false` caso contrário. E a função `=` retorna `true` se as duas datas forem iguais, `false` caso contrário.

Podemos também usar operadores lógicos para comparar datas. Por exemplo, podemos usar a função `and` para verificar se uma data está entre duas outras datas.

```Clojure
(let [data3 (java.time.LocalDateTime/of 2021 7 1) ; data específica: 1 de julho de 2021
      data4 (java.time.LocalDateTime/of 2021 9 1) ; data específica: 1 de setembro de 2021
      data5 (java.time.LocalDateTime/of 2021 8 15)] ; data específica: 15 de agosto de 2021
  (and (>= data5 data3) (<= data5 data4))) ; retorna true pois data5 está entre data3 e data4
```

## Veja também

- Documentação oficial de Clojure sobre a função `compare`: https://clojuredocs.org/clojure.core/compare
- Documentação oficial de Clojure sobre as funções `before?`, `after?` e `=`: https://clojuredocs.org/clojure.core/before%3F, https://clojuredocs.org/clojure.core/after%3F, https://clojuredocs.org/clojure.core/=
- Tutorial sobre manipulação de datas em Clojure: https://clojureverse.org/t/manipulating-datetime-in-clojure-the-de-facto-standard-jdk-api/5650/2