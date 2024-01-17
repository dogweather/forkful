---
title:                "Zapisywanie dużej litery ciągu znaków"
html_title:           "Clojure: Zapisywanie dużej litery ciągu znaków"
simple_title:         "Zapisywanie dużej litery ciągu znaków"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Kapitalizowanie stringów jest procesem zmiany pierwszej litery każdego słowa w ciągu znaków na wielką literę. Programiści robią to, aby stringi wyglądały czytelniej i poprawniej, szczególnie w kontekście wyświetlania wyjścia użytkownikowi.

## Jak to zrobić:

```Clojure
;; Dzięki funkcji clojure.string/capitalize możemy przekształcić stringa:
(clojure.string/capitalize "witaj świecie!")
;; zwraca "Witaj świecie!"

;; Można również skorzystać z funkcji re-find do odnalezienia słów w stringu i zmienić ich pierwszą literę:
(defn capitalize-words [str]
  (clojure.string/join " " (map #(clojure.string/capitalize %) (re-find #"\w+" str))))
(capitalize-words "witaj świecie!")
;; zwraca "Witaj Świecie!"
```

## Pogłębiona analiza:

Kapitalizowanie stringów ma swoje korzenie w sztuce pisania, gdzie stosuje się wielkie litery na początku zdań i nazw własnych. Alternatywą dla tej praktyki jest używanie wszystkich małych liter lub tzw. "camelCase" - którego pierwsza litera jest wielka a kolejne małe. W implementacji Clojure, funkcja capitalize korzysta z biblioteki Java, która używa standardowych reguł języka angielskiego do kapitalizacji stringów.

## Zobacz też:

[Dokumentacja funkcji clojure.string/capitalize](https://clojuredocs.org/clojure.string/capitalize)

[Porównanie różnych sposobów kapitalizacji stringów w języku Clojure](https://gist.github.com/Raynes/69339)