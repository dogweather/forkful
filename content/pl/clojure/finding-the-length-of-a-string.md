---
title:                "Clojure: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego?

Dlaczego chcielibyśmy dowiedzieć się długości ciągu znaków? Jest to przydatne w wielu przypadkach, na przykład w walidacji danych wejściowych lub w implementacji algorytmów szukania wzorca.

## Jak to zrobić?

Możemy użyć wbudowanej funkcji "count", która zwraca liczbę elementów w sekwencji lub kolekcji. W przypadku ciągu znaków, jest to po prostu jego długość.

```Clojure
(count "Hello world") ; Output: 11
(count "こんにちは") ; Output: 5
```

## Głębszy zanurzenie

Warto zauważyć, że funkcja "count" nie tylko działa na ciągach znaków, ale również na innych kolekcjach, takich jak listy, wektory czy mapy.

Ponadto, większość funkcji w Clojure jest "leniwa", co oznacza, że nie wykonują swojej operacji, dopóki nie są wymagane. W przypadku funkcji "count", może to oznaczać wywołanie jej na ogromnej sekwencji lub kolekcji, bez konieczności obliczania wszystkich jej elementów.

## Zobacz także

- https://clojuredocs.org/clojure.core/count
- https://clojure.org/about/sequences
- https://clojure.org/guides/learn/functions