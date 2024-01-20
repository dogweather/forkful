---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Obliczanie długości łańcucha to proces, który zwraca liczbę znaków w danym łańcuchu. Programiści robią to, aby kontrolować i manipulować danymi tekstowymi w ich aplikacjach.

## Jak to zrobić?

Wykorzystamy wbudowaną funkcję Clojure `count`. Oto prosty przykład:

```clojure
(defn string-length [s]
  (count s))
```

A teraz użyjmy tej funkcji:

```clojure
(string-length "Dzień dobry, Clojure!")
```

Wynik powyższego kodu to:

```
21
```

Podczas gdy używamy Unicode, wynik może być nieoczekiwany. Przykład:

```clojure
(string-length "Hello, Świat!")
```

Wynik powyżej to:

```
12
```
 
## W głąb tematu

Historia: Clojure, będąc dialektem Lisp, dziedziczy wiele z jego podejść do manipulacji łańcuchami, w tym obliczanie ich długości.

Alternatywy: Inne języki programowania mogą używać różnych metod do obliczania długości łańcucha, na przykład, w Javie jest to `string.length()`.

Szczegóły implementacji: Funkcja `count` w Clojure działa tak szybko, jak to tylko możliwe (czasami korzystając z wielowątkowych operacji). Funkcja ta zwraca ilość znaków, nie biorąc pod uwagę różnic Unicode.

## Zobacz także

- [Dokumentacja funkcji "count" z Clojure](https://clojuredocs.org/clojure.core/count)
- [Manipulacja łańcuchami w Clojure](https://clojure.org/guides/learn/strings) 
- [Porównanie funkcji `count` z innymi językami](https://www.tutorialspoint.com/count-function-in-clojure)