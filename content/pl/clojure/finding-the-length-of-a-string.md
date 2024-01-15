---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Clojure: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego
Dlaczego ktoś interesowałby się znajdowaniem długości ciągu? Istnieje wiele sytuacji, w których potrzebujemy określić liczbę znaków w stringu, na przykład w celu walidacji danych lub wyświetlenia informacji o wybranym tekście.

## Jak to zrobić
```Clojure
(def str "To jest przykładowy string")
(count str)
```

Przykładowy output: `25`

Innym sposobem jest użycie funkcji `length` na stringu, która również zwróci jego długość.

```Clojure
(length "Jesteśmy w 2021 roku")
```

Przykładowy output: `20`

Możemy również wykorzystać pętlę `for` w połączeniu z funkcją `count` w celu zliczenia długości każdego stringa w liście.

```Clojure
(def strings ["Cześć" "Hello" "Hola"])
(for [str strings]
  (count str))
```

Przykładowy output: `(5 5 4)`

## Głębszy wgląd
W Clojure, funkcja `count` może być używana nie tylko do znajdowania długości stringów, ale również innych kolekcji, takich jak listy, wektory czy mapy. Dzięki temu jest to uniwersalny sposób na obliczanie rozmiaru różnych typów danych w programie.

## Zobacz również
- [Dokumentacja Clojure: count](https://clojure.org/reference/sequences#count)