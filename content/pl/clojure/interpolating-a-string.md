---
title:                "Interpolacja ciągu znaków"
html_title:           "C++: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Interpolacja pozwala nam łączyć zmienne z literałami, tworząc tako dynamiczne ciągi znaków - przydatne, gdy chcemy wyświetlić różne dane w jednym miejscu. Jest to wygodne i oszczędza czas.

## Jak to zrobić?
W Clojure, można używać funkcji `format` do interpolowania stringów. Poniżej znajduje się przykładowy kod.

```clojure
(defn greet [name]
  (format "Hej %s, jak się masz?" name))

(println (greet "John"))
```
Po wykonaniu tego kodu, zostanie wyświetlony ciąg znaków: `"Hej John, jak się masz?"`.

## Dogłębniejsze spojrzenie
Historia: Interpolacja stringów istnieje praktycznie od początków języka programowania.  

Alternatywy: Istnieje wiele innych sposobów, np. konkatenacja (łączenie stringów) za pomocą operatora `+`. 

Informacje o implementacji: Clojure wykorzystuje Java String Formatter do implementacji funkcji `format`. Jest to dość wydajne, ale warto zauważyć, że użycie go może dodać dodatkową złożoność kodu, szczególnie gdy łączy się wiele zmiennych.

## Zobacz także

1. [Java String Formatter](https://docs.oracle.com/en/java/javase/13/docs/api/java.base/java/util/Formatter.html)
2. [Clojure String documentation](https://clojuredocs.org/clojure.core/format)