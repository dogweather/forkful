---
title:                "Comparando duas datas"
date:                  2024-01-20T17:32:52.004629-07:00
model:                 gpt-4-1106-preview
simple_title:         "Comparando duas datas"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O Quê & Por Quê?
Comparar duas datas é verificar se elas são iguais, qual vem antes ou depois. Programadores fazem isso para controlar eventos, prazos, e para lógica de negócios como validações de períodos de oferta ou monitoramento de datas de vencimento.

## Como fazer:
```Clojure
;; Adicionando a dependência clj-time
(require '[clj-time.core :as time])
(require '[clj-time.format :as format])

;; Definindo datas para comparação
(def data1 (time/date-time 2021 10 5))
(def data2 (time/date-time 2022 10 5))

;; Comparando as datas
(time/before? data1 data2) ; => true
(time/after? data1 data2) ; => false
(time/equal? data1 data2) ; => false

;; Formatando e comparando strings de datas
(def formato-br "dd/MM/yyyy")
(def data-str1 "05/10/2021")
(def data-str2 "05/10/2022")

(def parsed-data1 (time/parse (format/formatters formato-br) data-str1))
(def parsed-data2 (time/parse (format/formatters formato-br) data-str2))

(time/before? parsed-data1 parsed-data2) ; => true
```

## Aprofundamento:
Historicamente, a manipulação de datas em Clojure dependia de bibliotecas Java, como `java.util.Date`. Com o desenvolvimento de Clojure, surgiu a `clj-time`, uma biblioteca mais robusta, construída sobre a Joda-Time, que simplifica o trabalho com datas e horas.

Existem alternativas como o Java Time, que é parte do Java 8 e superior, requerendo menos dependências externas. No entanto, a `clj-time` ainda é amplamente usada por sua facilidade de uso na comunidade Clojure.

Detalhes de implementação importam. Comparar datas pode envolver zonas horárias e horário de verão. Use o UTC para consistência, a menos que contextos específicos exijam outro fuso. Além disso, ao comparar strings de datas, garantir que o formato esteja correto evita erros.

## Veja Também:
- Documentação clj-time: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- Biblioteca Java Time (java.time): [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- Guia para Joda-Time: [https://www.joda.org/joda-time/quickstart.html](https://www.joda.org/joda-time/quickstart.html)
