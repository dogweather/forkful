---
title:                "Obtendo a data atual"
html_title:           "Clojure: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Obter a data atual é uma tarefa comum em programação, pois permite que os desenvolvedores acessem informações relevantes do sistema, como registros e agendamentos. Além disso, pode ser usado para criar recursos como relógios e calendários.

## Como fazer:
```Clojure
(require '[clojure.java-time :as t])

; Data atual
(def current-date (t/local-date)) 

; Data atual em formato específico
(def formatted-date (t/formatter "MMMM dd, yyyy" (t/local-date)))

; Data atual com fuso horário
(def current-date-and-time (t/local-date-time (t/time-zone :default)))

; Data atual por país e cidade
(def local-date (-> gmt t/time-zone (t/local-date-time)))

; Data atual por continente e cidade
(def london-date (-> gb/europe t/time-zone (t/local-date-time)))
```

### Saída de exemplo:
```
current-date => 2021-10-11
formatted-date => October 11, 2021
current-date-and-time => #object[java.time.LocalDateTime 0x5e69c971 "2021-10-11T14:26:47.57"]
local-date => #object[java.time.LocalDateTime 0x261c23b2 "2021-10-11T05:26:47.57"]
london-date => #object[java.time.LocalDateTime 0x21d868be "2021-10-11T16:26:47.57"]
```

## Análise detalhada:
Obter a data atual é uma tarefa simples em Clojure, graças à biblioteca java-time integrada. Esta biblioteca é uma API para o java.time, que fornece recursos como conversão de fuso horário e formatação de data. Alternativamente, também é possível usar a biblioteca de terceiros clj-time para trabalhar com datas em Clojure.

## Leia também:
- [java-time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [clj-time library](https://github.com/clj-time/clj-time)