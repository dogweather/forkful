---
title:    "Haskell: Ekstrakcja podciągów"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu musimy operować na tekstach, wycinając z nich odpowiednie fragmenty. W języku Haskell istnieje wiele sposobów na wydobycie podciągów ze stringów. W tym artykule przekażę Wam kilka przydatnych trików, które pomogą Wam w tym zadaniu.

## Jak to zrobić

```Haskell
-- Użyj funkcji `take` do wydobycia pierwszych n znaków ze stringa
take :: Int -> [a] -> [a]

-- Przykład użycia:
take 4 "Haskell" -- Zwróci "Hask"

-- Użyj funkcji `drop` do pominięcia pierwszych n znaków ze stringa
drop :: Int -> [a] -> [a]

-- Przykład użycia:
drop 3 "Haskell" -- Zwróci "kell"

-- Użyj funkcji `splitAt` do podzielenia stringa na dwa podciągi na podstawie indeksu
splitAt :: Int -> [a] -> ([a], [a])

-- Przykład użycia:
splitAt 4 "Haskell" -- Zwróci ("Hask", "ell")

-- Możesz również użyć list comprehension do wydobycia podciągów
-- na podstawie warunków
[ x | x <- "Haskell", x `elem` "aieou" ] -- Zwróci "ae"

-- Funkcja `subsequences` zwraca wszystkie możliwe podciągi danego stringa
subsequences "abc" -- Zwróci ["","a","b","ab","c","ac","bc","abc"]
```

## Głębszy zanurzenie

Zanim zaczniemy używać funkcji do wyciągania podciągów, warto przyjrzeć się jej implementacji. W języku Haskell, stringi są reprezentowane jako listy znaków, więc wykorzystując funkcje związane z listami, możemy łatwo operować na stringach.

Funkcja `take` jest dosyć prosta - pobiera od nas liczbę znaków, które chcemy wydobyć oraz listę znaków, ze której chcemy je pobrać. Następnie zwraca nową listę zawierającą tylko pierwsze n znaków z oryginalnej listy.

Funkcje `drop` i `splitAt` działają podobnie, przyjmując odpowiednio liczbę znaków do pominięcia lub punkt podziału stringa.

Funkcja `subsequences` jest trochę bardziej skomplikowana, ponieważ musi zwrócić wszystkie możliwe kombinacje podciągów. Wykorzystuje do tego rekurencję i kształtuje listy zawierające wszystkie możliwe kombinacje.

## Zobacz również

- [Oficjalna dokumentacja Haskell](https://www.haskell.org/documentation/)
- [Tutorial dla początkujących w języku Haskell](https://learnxinyminutes.com/docs/pl-pl/haskell-pl/)
- [10 przykładów użycia list comprehension w języku Haskell](https://wiki.haskell.org/List_comprehension_examples)