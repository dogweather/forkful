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

## O que & Por quê?
Calculando uma data no futuro ou no passado refere-se a adicionar ou subtrair uma quantidade específica de tempo a uma data existente. Programadores frequentemente realizam esse cálculo ao criar aplicativos que lidam com eventos futuros ou passados, como agendas ou lembretes.

## Como fazer:
```Clojure
(require '[clj-time.core :as t])
(t/->DateTime (t/now) 
              (t/plus (t/days 10))) 
;=> #object[org.joda.time.DateTime 0x7f3ee01 "2021-09-13T00:00:00.000Z"]
```
O código acima mostra como usar a biblioteca "clj-time" para adicionar 10 dias à data atual e retornar o resultado. Você pode fazer cálculos semelhantes usando unidades de tempo como anos, meses, horas, minutos e segundos.

## Profundando mais:
Existem diversas formas de lidar com datas em aplicativos de programação, mas calcular uma data no futuro ou no passado é uma tarefa comum e importante. Uma alternativa seria usar a biblioteca "java.time" que vem com o Java 8 e versões mais recentes. Além disso, a implementação de calcular datas envolve tentar manter o ajuste para regras de ajuste de horário de verão e fuso horário, o que pode ser uma tarefa desafiadora.

## Veja também:
- [Documentação "clj-time"](https://cljdoc.org/d/clj-time/clj-time/0.14.4/doc/readme)
- [Artigo sobre "java.time"](https://www.baeldung.com/java-8-date-time-intro)