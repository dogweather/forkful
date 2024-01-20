---
title:                "Zamiana liter w ciągu na wielkie"
html_title:           "Clojure: Zamiana liter w ciągu na wielkie"
simple_title:         "Zamiana liter w ciągu na wielkie"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Kapitalizacja łańcuchów znaków w Clojure 

## Co i dlaczego?

Kapitalizacja łańcuchów znaków polega na zmianie pierwszego znaku w łańcuchu znaków na duży literę. Programiści robią to, by poprawić prezentację tekstu, np. przy wywietlaniu imion i nazwisk.

## Jak to zrobić:

Clojure (wersja obecna to 1.10) posiada wbudowaną funkcję `capitalize` w bibliotece `clojure.string`. Oto przykład:

```clojure
(require '[clojure.string :as str])

(defn show-capitalized [s]
   (str/capitalize s))

(println (show-capitalized "jan kowalski"))
```

Wynik:

```clojure
"Jan kowalski"
```

## Głębokie zanurzenie:

Kapitalizacja łańcuchów znaków jest praktyką stosowaną od początku informatyki. W Clojure jest to realizowane przez funkcje biblioteki `clojure.string`, która jest częścią standardowej dystrybucji Clojure.

Jest kilka alternatyw dla funkcji `capitalize`. Dwie wartościowe to `upper-case` i `lower-case`, które zmieniają wszystkie litery łańcucha znaków na duże lub małe litery.

Detale implementacji funkcji `capitalize` są dość proste. Funkcja dzieli łańcuch na dwa łańcuchy: pierwszy znak i resztę łańcucha. Następnie zamienia pierwszy znak na wielką literę i łączy go z resztą.

## Zobacz też:

Dokumentacja funkcji `capitalize` Clojure: [tutaj](https://clojuredocs.org/clojure.string/capitalize)
Blog na temat operacji na łańcuchach znaków w Clojure: [tutaj](https://www.braveclojure.com/core-functions-in-depth/)