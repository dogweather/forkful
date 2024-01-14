---
title:                "Clojure: Calculando uma data no futuro ou passado"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Existem várias situações em que pode ser útil calcular uma data no futuro ou no passado em um programa em Clojure. Por exemplo, ao lidar com agendamentos de tarefas, lembretes ou outras atividades que dependem de datas específicas.

## Como fazer

Para calcular uma data no futuro ou no passado em Clojure, podemos usar a função `clj-time.core/plus` do pacote `clj-time`. Primeiro, precisamos importar o pacote usando `(:require [clj-time.core :as time])`. Em seguida, podemos usar a função `plus` passando uma data inicial, uma quantidade de tempo e uma unidade de tempo como argumentos. Por exemplo:

```Clojure
(require '[clj-time.core :as time])
(time/plus (time/today) 3 :days)
```

O código acima irá retornar a data atual mais três dias no futuro. Podemos usar a mesma lógica para calcular uma data no passado, basta passar um número negativo como quantidade de tempo. Podemos usar diferentes unidades de tempo, como `:hours`, `:months`, `:years`, entre outras.

## Mergulhando fundo

Além da função `plus`, o pacote `clj-time` também possui outras funções úteis para trabalhar com datas, como `minus`, `to-date`, `start-of-day` e `end-of-day`. Além disso, é possível lidar com fusos horários e formatos de data personalizados usando outras bibliotecas, como o `clj-time.format` e o `clj-time.timezones`.

É importante lembrar que as datas em Clojure são imutáveis, o que significa que sempre que uma operação é realizada em uma data, uma nova data é criada. Isso garante consistência e evita erros de manipulação de data no código.

## Veja também

- Documentação oficial do pacote `clj-time`: https://github.com/clj-time/clj-time
- Tutorial sobre datas em Clojure: https://clojureverse.org/t/working-with-dates-in-clojure/5687
- Artigo sobre o pacote `clj-time` e como lidar com fusos horários: https://www.braveclojure.com/living-with-time-zones/