---
title:                "Wycinanie podciągów"
html_title:           "Clojure: Wycinanie podciągów"
simple_title:         "Wycinanie podciągów"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Wyciąganie podciągów jest procesem polegającym na wyodrębnieniu fragmentów tekstu (podciągów) z większego ciągu znaków. Programiści często wykorzystują tę technikę do manipulowania i przetwarzania tekstowych danych.

## Jak to zrobić:
### Przykład 1: 
```Clojure
(subs "Hello world" 0 5)
```
Output: "Hello"

### Przykład 2:
```Clojure
(subs "Lorem ipsum dolor sit amet" 6 11)
```
Output: "ipsum"

## Głębsze nurkowanie:
Wyciąganie podciągów jest powszechnie stosowaną operacją w wielu językach programowania. Jest to szczególnie przydatne w celu przetwarzania tekstów o nieznanej długości lub zawierających stałe wzorce. Istnieje wiele sposobów na ekstrakcję podciągów, w tym wykorzystanie funkcji wbudowanych lub zewnętrznych bibliotek.

## Zobacz również:
- Dokumentacja Clojure dotycząca funkcji (subs): https://clojuredocs.org/clojure.string/subs
- Alternatywne metody wyodrębniania podciągów w języku Clojure: https://www.codewars.com/kata/strings-and-chars/scala
- Oficjalny tutorial Clojure dla początkujących:https://clojure.org/guides/getting_started