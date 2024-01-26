---
title:                "Przetwarzanie daty ze łańcucha znaków"
date:                  2024-01-20T15:36:03.987340-07:00
html_title:           "Arduino: Przetwarzanie daty ze łańcucha znaków"
simple_title:         "Przetwarzanie daty ze łańcucha znaków"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parsowanie daty z ciągu znaków umożliwia konwersję tekstowej reprezentacji daty do formatu rozumianego przez komputer. Programiści wykonują tę czynność, aby móc manipulować datami, porównywać je i przetwarzać w aplikacjach.

## How to:
W Clojure daty parsujemy przy pomocy biblioteki `clj-time`, która oparta jest na Joda-Time. Oto jak to zrobimy:

```Clojure
(require '[clj-time.coerce :as coerce]
         '[clj-time.format :as format])

;; Definicja formatu daty
(def custom-formatter (format/formatters :date-time))

;; Parsowanie ciągu znaków do formatu daty
(defn parse-date [date-string]
  (coerce/from-string custom-formatter date-string))

;; Użycie funkcji
(parse-date "2023-04-01T15:30:00.000Z")
;; Wynik: #inst "2023-04-01T15:30:00.000-00:00"
```

## Deep Dive
Parsowanie dat ma długą historię, zwłaszcza w językach JVM, jak Java, gdzie Joda-Time ustawił standard jeszcze przed aktualnym java.time. W Clojure, `clj-time` jest popularnym wyborem ze względu na prostotę i wygodną integrację z Clojure. Warto jednak pamiętać o alternatywach jak `java.time` (Java 8+), teraz dostępna w Clojure bezpośrednio przez interop:

```Clojure
(import java.time.ZonedDateTime)

(ZonedDateTime/parse "2023-04-01T15:30:00.000Z")
;; Wynik: #object[java.time.ZonedDateTime 0x6d16eaaa "2023-04-01T15:30Z[UTC]"]
```

Dla większej kontroli nad formatowaniem, można wykorzystać `DateTimeFormatter` z tej samej biblioteki.

Detale implementacyjne warto poznać, aby np. obsłużyć różne strefy czasowe i formaty dat. `clj-time` i Java 8 `java.time` dostarczają silne narzędzia do pracy z czasem w aplikacjach.

## See Also
1. [clj-time GitHub repository](https://github.com/clj-time/clj-time)
2. [Java 8 java.time package documentation](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
3. [Joda-Time - Home](http://www.joda.org/joda-time/)
