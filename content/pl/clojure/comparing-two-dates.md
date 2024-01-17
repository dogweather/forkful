---
title:                "Porównywanie dwóch dat"
html_title:           "Clojure: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i dlaczego? 

Porównywanie dwóch dat w programowaniu to proces sprawdzania, czy jedna data jest wcześniejsza, późniejsza lub równa drugiej. Programiści wykonują to w celu porównania np. kolejności wystąpienia zdarzeń lub obliczenia różnicy w czasie.

## Jak to zrobić:

```Clojure
(def data1 (java.util.Date. 2020 12 31))
(def data2 (java.util.Date. 2021 1 1))

(> data1 data2) ;; false
(< data1 data2) ;; true
(= data1 data2) ;; false
```

## Głębszy wgląd:

Proces porównywania dat jest powszechnie stosowany w programowaniu, szczególnie w kontekście zarządzania i analizowania danych. Istnieją różne sposoby na porównywanie dat w różnych językach programowania, jednak w przypadku Clojure można korzystać z funkcji `>` (większy), `<` (mniejszy) i `=` (równy), które zwracają wartość logiczną `true` lub `false` dla danego warunku.

## Zobacz również:

- [Clojure Official Website](https://clojure.org/)
- [Java Date Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Comparing Dates in Python](https://realpython.com/python-datetime/)