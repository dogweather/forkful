---
title:                "Konwersja tekstu na małe litery"
html_title:           "Haskell: Konwersja tekstu na małe litery"
simple_title:         "Konwersja tekstu na małe litery"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek musiałeś(łaś) zmienić napis w programie na małe litery? Może zajęło Ci to sporo czasu, a może nadal nie wiesz, jak to zrobić. W tym artykule dowiesz się, jak w szybki i prosty sposób skonwertować napis na małe litery przy użyciu języka Haskell.

## Jak to zrobić

Aby skonwertować napis na małe litery w Haskell, użyj funkcji `map` i `toLower` z modułu `Data.Char`. Oto przykładowy kod z wykorzystaniem funkcji `map`:
```Haskell
import Data.Char (toLower)

lowercase :: String -> String
lowercase str = map toLower str
```
Wywołanie `lowercase "ABC"` zwróci napis `"abc"`. Możesz również użyć funkcji `map` wraz z funkcją `toLower` bezpośrednio w wyrażeniu:
```Haskell
lowercase str = map (\c -> toLower c) str
```

Jeśli chcesz, aby konwersja odbyła się wewnątrz funkcji, możesz wykorzystać funkcję `toLower` do pojedynczego znaku i funkcję `concatMap` do połączenia wyników w napis:
```Haskell
import Data.Char (toLower)

lowercase :: String -> String
lowercase str = concatMap (\c -> [toLower c]) str
```
Wywołanie `lowercase "ABC"` zwróci ten sam wynik, co poprzednie przykłady.

## Deep Dive

W Haskell jeden znak jest reprezentowany przez typ `Char`, dlatego używanie funkcji `toLower` na pojedynczym znaku zwróci znak w postaci `Char`. W celu otrzymania napisu, wykorzystujemy funkcję `concatMap`, która zwraca listę i łączy ją w jedno ciągłe napis.

Warto również wspomnieć, że funkcja `toLower` przekształca znaki tylko z alfabetu łacińskiego. Jeśli nasz napis zawiera znaki z innych alfabetów, nie zostaną one zmienione.

## Zobacz też

- Dokumentacja funkcji `map` w Haskell: https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:map
- Dokumentacja funkcji `toLower` w module `Data.Char`: https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html#v:toLower
- Poradnik "Jak zacząć z Haskell": https://pl.wikibooks.org/wiki/Haskell/Jak_zacz%C4%85%C4%87