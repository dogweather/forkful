---
title:                "Clojure: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que Comparar Duas Datas em Clojure?

Em programação, a necessidade de comparar datas é muito comum. Isso pode ser útil para verificar se um evento ocorreu antes ou depois de outro, ou para determinar o intervalo de tempo entre duas datas. Em Clojure, existem diversas maneiras de realizar essa comparação de forma eficiente e precisa.

## Como Fazer em Clojure

Existem várias funções em Clojure que podem ser usadas para comparar datas, como `before?`, `after?` e `compareTo`. Vamos ver alguns exemplos de como usá-las:

```
;; Criando duas datas
(def data1 (java.util.Date. 2021 9 1))
(def data2 (java.util.Date. 2021 10 1))

;; Verificando se data1 ocorreu antes de data2
(before? data1 data2)
;; Output: true

;; Verificando se data1 ocorreu depois de data2
(after? data1 data2)
;; Output: false

;; Comparando as duas datas
(compareTo data1 data2)
;; Output: -1 (data1 é anterior a data2)
```


Além disso, também é possível comparar datas usando os operadores `<`, `>` e `=`. Porém, é importante lembrar que esses operadores retornam `true` ou `false` sem considerar as horas, minutos e segundos das datas. Por isso, é recomendado utilizar as funções específicas de comparação de datas.

## Aprofundando no Comparativo de Datas

Em Clojure, as datas são representadas pelo tipo `java.util.Date`. Isso significa que elas são objetos e possuem diversos métodos que podem ser utilizados para manipulá-las. Além disso, existem também outras bibliotecas externas que facilitam o trabalho com datas em Clojure, como a `clj-time` e a `time-literals`.

Outro conceito importante ao comparar datas é a zona de tempo ou fuso horário. Em Clojure, é possível definir qual é a zona de tempo padrão para seus cálculos, utilizando a função `set!`, por exemplo: `(set! *time-zone* "Europe/Lisbon")`. Isso garante que as comparações serão feitas levando em consideração o fuso horário desejado.

## Veja Também

- [Documentação oficial de funções de datas em Clojure](https://clojuredocs.org/clojure.core/before_q)
- [Liberação do tempo em Clojure usando clj-time](https://github.com/clj-time/clj-time)
- [Manipulação de datas com time-literals](https://github.com/onetom/time-literals)