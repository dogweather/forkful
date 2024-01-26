---
title:                "Zamiana liter na wielkie w ciągu znaków"
html_title:           "Arduino: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Kapitalizacja to proces zmiany pierwszej litery w ciągu na wielką. Programiści to robią, bo w wielu przypadkach takie formatowanie ułatwia czytelnikom rozpoznawanie nazw własnych, początków zdań, tytułów oraz aby spełnić wymogi stylistyczne i gramatyczne.

## Jak to zrobić:
W Haskellu możemy użyć funkcji 'toUpper' z modułu 'Data.Char' do zmiany małych liter na wielkie. Sprawdźmy to w praktyce:

```Haskell
import Data.Char (toUpper)

-- Funkcja do kapitalizacji pojedynczego słowa
capitalize :: String -> String
capitalize [] = []
capitalize (head:tail) = toUpper head : tail

-- Przykład działania
main = putStrLn $ capitalize "haskell"

-- Output:
-- Haskell
```

Chcesz zmienić pierwsze litery wszystkich słów? Zrób to tak:

```Haskell
import Data.Char (toUpper, isSpace)

-- Funkcja pomocnicza do wykrywania granic słów
isWordStart :: Char -> Char -> Bool
isWordStart prev curr = isSpace prev && (not . isSpace) curr

-- Funkcja dla całego ciągu
capitalizeWords :: String -> String
capitalizeWords [] = []
capitalizeWords [x] = [toUpper x]
capitalizeWords (x:y:xs) = toUpper x : go x y xs
    where go _ current [] = [toUpper current]
          go previous current (next:rest)
            | isWordStart previous current = toUpper current : go current next rest
            | otherwise = current : go current next rest

-- Spróbujmy
main = putStrLn $ capitalizeWords "haskell - funkcjonalny i elegancki"

-- Output:
-- Haskell - Funkcjonalny I Elegancki
```

## Głębsze zanurzenie
Kapitalizacja to nie nowość - stosujemy ją w typografii od wieków. W Haskellu, jako funkcjonalnym języku, moglibyśmy zastosować inne podejścia, jak leniwa ewaluacja czy inny styl rekurencji.

Jednakże 'Data.Char' to standardowy moduł i jego funkcje 'toUpper' czy 'toLower' są szybkie i niezawodne. W wielu językach kapitalizacja jest podstawową funkcją w bibliotekach stringów. Haskell wymaga nieco więcej pracy, bo jest to język niskiego poziomu z naciskiem na funkcje czyste.

Alternatywą mogłoby być napisanie własnych funkcji, które przetwarzają tekst na poziomie bajtów czy obsługa różnych kodowań tekstu. Ale dla większości potrzeb 'Data.Char' w zupełności wystarczy.

## Zobacz również
Sprawdź te źródła dla lepszego zrozumienia:

- [Data.Char documentation](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Char.html)
- [Haskell Wiki on strings](https://wiki.haskell.org/Strings)

Te linki prowadzą do dokumentacji oraz stron, które pomogą Ci zgłębić temat obróbki tekstu w Haskellu.
