---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Haskell: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Znajdowanie długości ciągu znaków to podstawowa operacja w programowaniu, polegająca na zliczeniu wszystkich znaków w danym ciągu. Jest to często wykorzystywane do analizy i przetwarzania danych tekstowych. Programiści używają jej często, ponieważ jest to niezbędna czynność przy pracy z tekstami.

## Jak to zrobić:
Aby obliczyć długość ciągu znaków w Haskellu, możemy użyć funkcji `length`. Przyjmie ona jako argument nasz ciąg znaków i zwróci jego długość.

```Haskell
length "Hello, world!" -- output: 13
```

Możemy także łączyć funkcje, np. korzystając z wcześniej omówionej funkcji `length` i funkcji `show`, aby wyświetlić długość ciągu w postaci tekstowej.

```Haskell
show (length "Hello, world!") -- output: "13"
```

## Głębsze zagadnienia:
Obliczanie długości ciągu znaków jest operacją powszechnie stosowaną w programowaniu od lat. Wcześniej, w tradycyjnych językach programowania, musieliśmy używać pętli i liczników, aby wykonać tę czynność. Jednak w Haskellu, funkcja `length` jest wbudowana i wykonuje to zadanie w wydajny sposób.

Alternatywną metodą obliczania długości ciągu jest użycie funkcji `foldl`, która agreguje elementy listy w jedną wartość, przyjmując jako argument funkcję, która wykonuje operację na elementach listy. Jednak dla krótkich ciągów znaków nie ma się to dużego znaczenia, a nawet może być mniej wydajne od użycia funkcji `length`.

## Zobacz też:
- [Oficjalna dokumentacja dla funkcji `length`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:length)
- [Inne przydatne funkcje w Haskellu](https://wiki.haskell.org/Standard_library)
- [Zastosowanie funkcji `length` w praktyce](https://dev.to/itseranga/5-must-know-string-operations-in-haskell-15l8)