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

## Dlaczego

Każdy programista wie, że regularne wyrażenia są nieodłącznym elementem pracy z tekstem. Znajomość ich składni i użycie może znacznie ułatwić i przyspieszyć pracę, dlatego warto poznać podstawy ich działania.

## Jak to zrobić

```Clojure
;; Przykładowy ciąg znaków
(def string "Alice ma 21 lat, Bob ma 29 lat, Chris ma 43 lata.")

;; Wyszukanie wszystkich liczb w tekście
(re-seq #"\d+" string)

;; Output: ("21" "29" "43")
```

```Clojure
;; Zamiana daty z formatu MM/DD/RRRR na DD-MM-RRRR
(def data "09/30/2021")
(re-sub #"(\d{2})/(\d{2})/(\d{4})" data "$2-$1-$3")

;; Output: "30-09-2021"
```

```Clojure
;; Sprawdzenie czy ciąg zawiera email
(def email "johndoe@email.com")
(re-match #"\w+@\w+\.\w+" email)

;; Output: "johndoe@email.com"
```

## Pogłębiona analiza

Podstawowym elementem regularnych wyrażeń jest wzorzec, którym chcemy dopasować ciąg znaków. Najczęściej używanymi są:
- Znaki literowe - reprezentujące konkretne litery lub ciągi znaków, np. `a` lub `abc`.
- Metaznaki - służące do reprezentacji grup znaków, np. `.` dla jednego dowolnego znaku, `+` dla jednego lub więcej znaków, `*` dla zera lub więcej znaków.
- Klasy znaków - pozwalające na określenie zakresu znaków, np. `[a-z]` dla liter od A do Z, `[0-9]` dla cyfr od 0 do 9.
- Sekwencje specjalne - reprezentujące konkretne znaki, np. `\s` dla spacji, `\d` dla cyfr.

Regularne wyrażenia mogą być również wykorzystywane do modyfikacji tekstu, dzięki użyciu grup i specjalnych ciągów zamieniających.

## Zobacz także

- [Podstawowe wyrażenia regularne w Clojure](https://www.tutorialspoint.com/clojure/clojure_regular_expressions.htm)
- [Inne przydatne funkcje Clojure do pracy z tekstem](https://clojure.org/api/cheatsheet)