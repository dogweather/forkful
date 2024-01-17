---
title:                "Używanie wyrażeń regularnych"
html_title:           "Clojure: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Regular expressions są wyrażeniami służącymi do wyszukiwania wzorców w tekście. Programiści używają ich, aby skutecznie manipulować oraz wyciągać potrzebne informacje ze złożonych danych.

## Jak to zrobić:
Przykłady kodu poniżej pokazują, jak użyć regular expressions w Clojure.

```Clojure
;; Przykładowy tekst
(def tekst "Ala ma kota, ale kot nie ma Ali.")

;; Wyszukaj wszystkie wystąpienia słowa 'kot'
(re-seq #"kot" tekst)
;; Output: ("kot" "kot")

;; Zastąp każde wystąpienie słowa 'kot' na 'pies'
(re.sub #"kot" "pies" tekst)
;; Output: Ala ma kota, ale pies nie ma Ali.
```

## Głębsza analiza:
Regular expressions pierwotnie zostały opracowane przez matematyka Stephena Kleene w latach 50. jako sposób na opisywanie języków formalnych. Alternatywami dla regular expressions są inne wyrażenia, takie jak pobieranie podciągów lub użycie metody "contains" na tekście. Implementacja regular expressions w Clojure wykorzystuje dostępne funkcje języka, takie jak "re-find" czy "re-seq".

## Zobacz też:
Dla lepszego zrozumienia i wykorzystania regular expressions w Clojure, warto zapoznać się z dokumentacją oficjalną: https://clojure.github.io/clojure/clojure.string-api.html#clojure.core/re-seq oraz z tutorialami dostępnymi online, takimi jak ten: https://purelyfunctional.tv/guide/clojure-regex/