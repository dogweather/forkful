---
title:                "Znalezienie długości ciągu znaków"
date:                  2024-01-20T17:47:18.254753-07:00
model:                 gpt-4-1106-preview
simple_title:         "Znalezienie długości ciągu znaków"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Długość łańcucha to liczba znaków w tekście. Programiści liczą ją, by zarządzać danymi tekstowymi – np. sprawdzić, czy hasło ma odpowiednią długość lub wyświetlić fragment większego tekstu.

## Jak zrobić:
```clojure
;; Użyj funkcji count do znalezienia długości stringa
(count "Cześć, jak się masz?")
;; Wynik: 19

;; Lub użyj funkcji .length na obiekcie typu String
(.length "Dzień dobry!")
;; Wynik: 12
```

## Głębsze spojrzenie
Funkcja `count` w Clojure działa nie tylko na stringach, ale i na innych kolekcjach. Historia Clojure, języka z rodziny Lisp, podkreśla prostszą, ale potężną składnię – tak samo jest z funkcją `count`. Oprócz `count`, od Javy możemy dziedziczyć metodę `.length`.

Alternatywą jest używanie biblioteki `clojure.string` dla operacji specyficznych dla stringów, która jednak do liczenia znaków nie jest konieczna.

Szczegółowo: `count` działa w O(1) dla stringów – to znaczy, że zawsze będzie szybka, niezależnie od długości stringa. Metoda `.length` również działa w stałym czasie, bo jest to właściwość klasy String w Javie.

## Zobacz również
- Dokumentacja funcji `count`: [https://clojuredocs.org/clojure.core/count](https://clojuredocs.org/clojure.core/count)
- Dokumentacja Clojure `clojure.string`: [https://clojuredocs.org/clojure.string](https://clojuredocs.org/clojure.string)
- Informacje o Javie i jej metodzie `.length`: [https://docs.oracle.com/en/java/](https://docs.oracle.com/en/java/)
