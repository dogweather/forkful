---
title:                "Wykorzystanie wyrażeń regularnych"
html_title:           "Arduino: Wykorzystanie wyrażeń regularnych"
simple_title:         "Wykorzystanie wyrażeń regularnych"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Regular expressions (regexpy) to wzorce używane do dopasowywania ciągów znaków. Programiści wykorzystują je do wyszukiwania, walidacji, edycji i ekstrakcji danych z tekstu.

## Jak to zrobić:
```Clojure
;; Wyszukiwanie dopasowań
(re-find #"\d+" "123 abc") ; Znajduje pierwszą sekwencję cyfr
; Wynik: "123"

;; Walidacja formatu
(boolean (re-matches #"\d{2}-\d{3}" "12-345")) ; Sprawdza, czy tekst jest kodem pocztowym
; Wynik: true

;; Podział ciągu znaków
(re-seq #"[A-Za-z]+" "abc 123 def") ; Dzieli tekst na słowa
; Wynik: ("abc" "def")

;; Zamiana za pomocą wyrażeń regularnych
(clojure.string/replace "abc123" #"\d" "*")
; Wynik: "abc***"
```

## Dogłębna analiza:
Regexy sięgają lat 50. XX wieku, a ich obecna forma wywodzi się z formalnej teorii automatów i języków formalnych. W Clojure używa się java.util.regex, bo Clojure działa na JVM. Alternatywy dla regexów to parsery i biblioteki do przetwarzania tekstów, jak np. `instaparse`. Implementacja regexów w Clojure jest wydajna, lecz nie zawsze przejrzysta przy skomplikowanych wzorcach.

## Zobacz również:
1. [ClojureDocs - Regular Expressions](https://clojuredocs.org/clojure.core/re-find)
3. [Regular-Expressions.info](https://www.regular-expressions.info/tutorial.html) - ogólne informacje o regexach, niezależnie od języka.