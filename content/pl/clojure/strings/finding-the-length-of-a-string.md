---
date: 2024-01-20 17:47:18.254753-07:00
description: "Jak zrobi\u0107: Funkcja `count` w Clojure dzia\u0142a nie tylko na\
  \ stringach, ale i na innych kolekcjach. Historia Clojure, j\u0119zyka z rodziny\
  \ Lisp, podkre\u015Bla\u2026"
lastmod: '2024-04-05T21:53:36.432211-06:00'
model: gpt-4-1106-preview
summary: "Funkcja `count` w Clojure dzia\u0142a nie tylko na stringach, ale i na innych\
  \ kolekcjach."
title: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w"
weight: 7
---

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
