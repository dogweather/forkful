---
date: 2024-01-20 17:47:18.254753-07:00
description: "D\u0142ugo\u015B\u0107 \u0142a\u0144cucha to liczba znak\xF3w w tek\u015B\
  cie. Programi\u015Bci licz\u0105 j\u0105, by zarz\u0105dza\u0107 danymi tekstowymi\
  \ \u2013 np. sprawdzi\u0107, czy has\u0142o ma odpowiedni\u0105 d\u0142ugo\u015B\
  \u0107 lub\u2026"
lastmod: '2024-03-13T22:44:34.985428-06:00'
model: gpt-4-1106-preview
summary: "D\u0142ugo\u015B\u0107 \u0142a\u0144cucha to liczba znak\xF3w w tek\u015B\
  cie. Programi\u015Bci licz\u0105 j\u0105, by zarz\u0105dza\u0107 danymi tekstowymi\
  \ \u2013 np. sprawdzi\u0107, czy has\u0142o ma odpowiedni\u0105 d\u0142ugo\u015B\
  \u0107 lub\u2026"
title: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w"
weight: 7
---

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
