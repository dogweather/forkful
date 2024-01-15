---
title:                "Comparando duas datas"
html_title:           "Clojure: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que

Comparar duas datas pode ser útil em diferentes situações, como determinar se uma data é anterior ou posterior a outra, calcular a diferença de tempo entre as datas ou verificar se um evento ocorreu antes ou depois de outra data. Além disso, é uma habilidade importante para qualquer desenvolvedor que trabalha com dados temporais ou precisa lidar com agendamentos ou cronogramas.

## Como Fazer

A comparação de datas em Clojure é bastante simples. Primeiro, precisamos criar duas variáveis contendo as datas que desejamos comparar, usando a função `clj-time` para criar objetos de data.

```clojure
(require '[clj-time.core :as time])

(def data1 (time/date 2021 5 1))
(def data2 (time/date 2020 11 30))
```

Agora, podemos usar as funções `before?` e `after?` para comparar as datas. A função `before?` retorna `true` se a primeira data é anterior à segunda data, enquanto a função `after?` retorna `true` se a primeira data é posterior à segunda data.

```clojure
(time/before? data1 data2)  ;; retorna false
(time/after? data1 data2)   ;; retorna true
```

Se quisermos verificar se as datas são iguais, podemos usar a função `equals?`, que retorna `true` se as duas datas forem idênticas.

```clojure
(time/equals? data1 data2)  ;; retorna false
(time/equals? data1 data1)  ;; retorna true
```

Também é possível calcular a diferença entre as datas usando a função `period-between`, que retorna um período em dias, meses e anos.

```clojure
(def diferenca (time/period-between data1 data2))
;; {:days -153, :months -6, :years -1}
```

## Deep Dive

Na verdade, as datas são comparadas como objetos de tempo, que têm uma precisão de segundos. Isso significa que, se duas datas tiverem a mesma hora, mas uma delas tiver um atraso de alguns segundos, elas ainda serão consideradas iguais ao usar a função `equals?`.

Além disso, a função `before?` retorna `true` mesmo se as datas tiverem o mesmo dia, mês e ano, desde que a primeira data seja anterior no tempo. Isso pode ser um pouco confuso, mas é importante entender que a comparação de datas leva em consideração a precisão de segundos.

## Veja Também

- [Documentação oficial do clj-time](https://clj-time.github.io/clj-time/)
- [Funções de data e hora em Clojure](https://clojuredocs.org/clojure.core/time)
- [Dica de Clojure: Usando clj-time para manipulação de data e hora](https://www.londatiga.net/it/clojure-tip-using-clj-time-for-datetime-manipulation/)