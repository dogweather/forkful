---
title:                "Haskell: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie w języku Haskell może wydawać się trudne dla niektórych, ale jest to język o wielu zaletach. Jedną z nich jest możliwość wykorzystania list do przechowywania danych i operacji na nich. Dzięki temu możemy wykonać wiele ciekawych zadań, takich jak obliczanie długości ciągu znaków. W tym artykule wyjaśnimy, dlaczego warto nauczyć się znajdowania długości ciągów w języku Haskell.

## Jak to zrobić

```Haskell
length :: [a] -> Int
```

Aby obliczyć długość ciągu znaków w Haskellu, możemy użyć funkcji "length". Ta funkcja przyjmuje jako argument listę elementów (w tym przypadku ciąg znaków) i zwraca liczbę całkowitą reprezentującą długość tej listy.

```Haskell
length "Hello World" -- zwróci 11
length "" -- zwróci 0
```

W powyższych przykładach, funkcja "length" zwróciła odpowiednio 11 i 0, ponieważ pierwszy argument to ciąg znaków składający się z 11 liter, a drugi argument to pusty ciąg znaków.

## Głębsze zagadnienia

Można zauważyć, że funkcja "length" przyjmuje listę elementów o dowolnym typie (oznaczonego jako "a") i zwraca liczbę całkowitą. Dzięki temu, możemy wykorzystać tę funkcję do obliczania długości różnego rodzaju list, nie tylko ciągów znaków.

Ponadto, warto zwrócić uwagę na fakt, że funkcja "length" jest bardzo wydajna. Dzięki zastosowaniu techniki "lazy evaluation", funkcja ta oblicza długość listy bez konieczności przejścia przez wszystkie jej elementy. Jest to przydatne, gdy mamy do czynienia z dużymi listami.

## Zobacz też

- [Funkcje w języku Haskell](https://www.codingame.com/playgrounds/6231/funkcje-w-haskell)
- [Rekursja w języku Haskell](https://wiki.haskell.org/Rekursja)
- [Porównanie wydajności funkcji "length" w Haskellu i innych językach](https://codereview.stackexchange.com/questions/120960/comparing-length-functions-for-strings-in-some-haskell-and-javascript-variations)