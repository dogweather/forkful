---
title:                "Clojure: Konwersja daty na ciąg znaków."
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu spotykamy się z koniecznością przetworzenia daty na ciąg znaków, który będzie czytelny dla użytkownika. W tym wpisie dowiesz się, jak w prosty sposób wykonać tę konwersję w języku Clojure.

## Jak to zrobić

Pierwszym krokiem jest zaimportowanie biblioteki `clj-time`, która udostępnia funkcje do pracy z datami. Następnie, używając funkcji `format`, możemy przekształcić datę w wybrany przez nas format. Poniżej znajdują się przykładowe użycia tej funkcji:

```Clojure
(ns date-to-string.core
  (:require [clj-time.format :as fmt]))

(fmt/format "dd MMMM yyyy" (clj-time.core/now))
;; Output: "10 października 2021"

(fmt/format "dd-MM-yyyy" (clj-time.core/now))
;; Output: "10-10-2021"
```
Funkcja `format` przyjmuje dwa argumenty - format, na jaki chcemy przekształcić datę oraz samą datę.

Możemy również wykorzystać funkcję `parse` do przekształcenia ciągu znaków w datę. Przykład:

```Clojure
(ns date-to-string.core
  (:require [clj-time.format :as fmt]))

(fmt/parse (clj-time.format/datetime-formatter "dd-MM-yyyy") "10-10-2021")
;; Output: #object[org.joda.time.DateTime 0xb5d7ce9f "2021-10-10T00:00:00.000Z"]
```

## Głębsze zagadnienia

Podczas pracy z datami warto zwrócić uwagę na użycie funkcji `format-local` i `parse-local` zamiast `format` i `parse`. Jest to zalecane w przypadku, gdy pracujemy z datami w różnych strefach czasowych. Ponadto, biblioteka `clj-time` oferuje wiele innych funkcji do pracy z datami, takich jak `plus`, `minus`, czy `duration`.

## Zobacz również

- Dokumentacja biblioteki `clj-time`: https://clj-time.github.io/clj-time/
- Konwersja typów danych w języku Clojure: https://clojure.org/guides/spec#_converting_between_types
- Praca z datami za pomocą biblioteki `java.time` w Clojure: https://www.baeldung.com/java-time-clojure