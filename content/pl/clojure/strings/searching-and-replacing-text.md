---
date: 2024-01-20 17:57:29.505163-07:00
description: "How to: (Jak to zrobi\u0107:) Wyszukiwanie i zamiana tekstu w Clojure\
  \ dzia\u0142a zgodnie z mechanizmami wykorzystuj\u0105cymi wyra\u017Cenia regularne\
  \ z Javy. Pojawi\u0142o si\u0119\u2026"
lastmod: '2024-04-05T22:50:49.287507-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Wyszukiwanie i zamiana tekstu w Clojure dzia\u0142\
  a zgodnie z mechanizmami wykorzystuj\u0105cymi wyra\u017Cenia regularne z Javy."
title: Wyszukiwanie i zamiana tekstu
weight: 10
---

## How to: (Jak to zrobić:)
```Clojure
; Znajdowanie tekstu z użyciem re-find
(re-find #"Clojure" "Clojure to świetny język programowania")

; Wyjście: "Clojure"

; Zamiana tekstu przy pomocy re-seq i replace
(clojure.string/replace "Clojure to fajny język" #"fajny" "świetny")

; Wyjście: "Clojure to świetny język"

; Jeśli musisz zamienić wiele wystąpień:
(clojure.string/replace "Java to OK, ale Clojure to OK." #"OK" "najlepszy")

; Wyjście: "Java to najlepszy, ale Clojure to najlepszy."
```

## Deep Dive (Dogłębniejsze spojrzenie)
Wyszukiwanie i zamiana tekstu w Clojure działa zgodnie z mechanizmami wykorzystującymi wyrażenia regularne z Javy. Pojawiło się to po raz pierwszy w lispach pod koniec lat 50. XX wieku. Alternatywy obejmują filtrację i przetwarzanie kolekcji za pomocą `map` i `filter`. Praktyczne implementacje wymagają zrozumienia wyrażeń regularnych i specyfikacji Clojure w zakresie obsługi stringów.

## See Also (Zobacz także)
- [Clojure Documentation for clojure.string](https://clojure.github.io/clojure/clojure.string-api.html)
- [Clojure Regular Expressions](https://www.braveclojure.com/functional-programming/#Regular_Expressions)
- [Oracle JavaDoc for Pattern class](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
