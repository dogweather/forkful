---
title:                "Wydobywanie podciągów"
html_title:           "Python: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wyciąganie podciągów to proces odcinania jednej lub więcej ciągów znaków z istniejącego ciągu. Programiści to robią, aby ułatwić manipulację danymi i zwiększyć efektywność kodu.

## Jak to zrobić:

Oto kilka podstawowych przykładów, jak można to zrobić:

```Haskell
-- Używając funkcji drop i take
substring :: Int -> Int -> String -> String
substring start end = drop start . take end

-- To jest jak to działa:
ghci> substring 0 5 "Hello, World!"
"Hello"

-- Używając list comprehension
substring_lc :: Int -> Int -> String -> String
substring_lc start end str = [str !! i | i <- [start..(end-1)]]

ghci> substring_lc 0 5 "Hello, World!"
"Hello"
```
Możemy też używać funkcji wbudowanych, takich jak `take`, `drop` i `splitAt` do manipulowania ciągami znaków.

## Głębsze spojrzenie:

Ciągi znaków są fundamentalnym typem danych w Haskell, z dużą ilością operacji wbudowanych. Wyciągniecie podciągu może wydawać się proste, ale była to znaczna zmiana w długiej historii języków programowania.

Alternatywą dla wyciągania podciągu w Haskellu może być użycie wyrażeń regularnych, jednakże może ono być bardziej skomplikowane i trudne do optymalizacji.

Z punktu widzenia implementacji, zarówno `take` jak i `drop` są dosyć proste, ponieważ są one jednymi z podstawowych operacji na listach, które są podstawowym typem danych w Haskellu. Haskell wykonuje te operacje leniwie, co oznacza, że nie oblicza wartości aż do momentu, gdy jest to absolutnie konieczne.

## Zobacz też:

- [Dokumentacja funkcji `take`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:take)
- [Dokumentacja funkcji `drop`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:drop)
- [Dokumentacja funkcji `splitAt`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:splitAt)