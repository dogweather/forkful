---
title:                "Comparando duas datas"
html_title:           "C#: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Comparações de Datas em Clojure

## O Que é & Porquê?

Temos o ato de comparar duas datas, usado frequentemente para determinar a diferença entre elas, ordenar datas e verificar a igualdade. Programadores o utilizam para algoritmos de agendamento, logging, e mais.

## Como Fazer:

As comparações de datas em Clojure podem ser feitas usando a função `compare`. Aqui está um exemplo de como você pode comparar duas datas:

```clojure
(require '[clj-time.core :as t]
         '[clj-time.coerce :as c])

(let [date1 (t/date-time 2020 7 3)
      date2 (t/date-time 2020 7 4)]
  (compare date1 date2))
```

A saída disso seria `-1`, significando que a primeira data é anterior à segunda. Se as datas fossem iguais, a saída seria `0`. Se a primeira data fosse posterior à segunda, a saída seria `1`.

## Aprofundamento:

**Contexto Histórico**

A função `compare` foi introduzida no Clojure para fornecer uma comparação consistente entre diferentes tipos de dados, incluindo datas.

**Alternativas**

Você pode também usar a função `before?` ou `after?` do clj-time para comparar duas datas:

```clojure
(require '[clj-time.core :as t])

(let [date1 (t/date-time 2020 7 3)
      date2 (t/date-time 2020 7 4)]
  (t/before? date1 date2)) ;; Retorna true se a primeira data estiver antes da segunda
```

Isso retornará `true` se `date1` for antes de `date2`, e `false` caso contrário.

**Detalhes de Implementação**

A função `compare` compara os componentes individuais das datas na seguinte ordem: ano, mês, dia, hora, minuto, segundo e milissegundo.