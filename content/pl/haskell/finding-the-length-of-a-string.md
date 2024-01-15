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

## Dlaczego

Gdy piszesz programy, często potrzebujesz określić długość tekstu. W Haskellu, Możesz użyć funkcji `length` aby łatwo znaleźć długość łańcucha znaków.

## Jak to zrobić

Użyj funkcji `length` podając łańcuch znaków jako argument, a następnie wydrukuj wynik na ekranie. Proste, prawda?

```Haskell
length "Hello World!" -- zwraca 12
```

## Głębsza analiza

Funkcja `length` w Haskellu jest częścią standardowej biblioteki i jest bardzo użyteczna w różnych zastosowaniach. Jej typ jest następujący: `length :: [a] -> Int`, co oznacza, że przyjmuje listę dowolnego typu `a` i zwraca liczbę całkowitą (`Int`).

W praktyce, możesz użyć `length` na różnych typach danych, które są instancjami typu klasy `Foldable`, takie jak listy, wektory, czy nawet tablice. Funkcja ta działa rekurencyjnie, przez co jest bardzo wydajna, ponieważ nie musisz przechodzić całej listy od początku.

## Zobacz też

- [Dokumentacja Funkcji `length` w Haskellu](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:length)
- [Tutorial dla początkujących w Haskellu](https://wiki.haskell.org/Tutorials)
- [Inne funkcje dla manipulacji list w Haskellu](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html)