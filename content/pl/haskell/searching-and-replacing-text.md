---
title:                "Haskell: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Często jesteśmy zmuszeni do ręcznego zmieniania dużej ilości tekstu, na przykład gdy musimy wykonać powtarzalną operację na kilku plikach. W takiej sytuacji wykorzystanie funkcji wyszukiwania i zamiany tekstu w języku Haskell jest bardzo wygodnym i efektywnym sposobem na rozwiązanie tego problemu.

## Jak To Zrobić

Używając funkcji `sub` z biblioteki `Text.Regex.PCRE`, możemy przeprowadzić proste wyszukiwanie i zamianę tekstu w języku Haskell. W poniższym przykładzie zamienimy wszystkie wystąpienia słowa "Witaj" na "Cześć" w tekście:

```Haskell
import Text.Regex.PCRE

main = do
  let text = "Witaj, to jest tekst powitalny."
  let newText = sub regex "Cześć" text
  putStrLn newText
  where
    regex = makeRegex "Witaj" :: Regex
```

Wynikiem działania tego programu będzie:

```
Cześć, to jest tekst powitalny.
```

Używając funkcji `gsub` zamiast `sub`, możemy zamienić wszystkie wystąpienia danego słowa, a nie tylko pierwsze. 

## Głębszy Wgląd

Funkcja `sub` oraz `gsub` działają na podobnej zasadzie jak funkcja `replace` z biblioteki `Data.Text`, jednak korzystają z wyrażeń regularnych. Wykorzystując wyrażenia regularne, możemy dokładniej kontrolować, które fragmenty tekstu mają zostać zamienione, na przykład mogą być to tylko wystąpienia danego słowa, podane litery, liczby czy inne wzorce. Dodatkowo, korzystając z funkcji `makeRegex` możemy zbudować wyrażenie regularne z dowolnego tekstu zawierającego zmienne. 

## Zobacz Również

- Dokumentacja funkcji `sub` z biblioteki `Text.Regex.PCRE`: http://hackage.haskell.org/package/regex-pcre/docs/Text-Regex-PCRE.html#v:sub
- Poradnik wyrażeń regularnych w Haskellu: https://en.wikibooks.org/wiki/Haskell/Understanding_the_regular_expression_package
- Przykładowe zadania korzystające z funkcji wyszukiwania i zamiany tekstu: https://exercism.io/tracks/haskell/exercises/grade-school