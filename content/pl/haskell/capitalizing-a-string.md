---
title:                "Zamiana małych liter na duże w łańcuchach znaków"
html_title:           "Haskell: Zamiana małych liter na duże w łańcuchach znaków"
simple_title:         "Zamiana małych liter na duże w łańcuchach znaków"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zmiana pierwszego znaku ciągu na wielką literę to operacja związana z formatowaniem stringów. Programiści często robią to, by poprawić czytelność wyników, na przykład w tytułach lub na początku zdań.

## Instrukcja krok po kroku:
Możemy to zrobić w Haskellu za pomocą wbudowanej funkcji `toUpper` z modułu `Data.Char` oraz `map` i list comprehension. Przykładowy kod:

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize "" = ""
capitalize (head:tail) = toUpper head : tail
```
Wyprowadzenie:
```Haskell
capitalize "witam w świecie haskella"
-- Wyprowadza: "Witam w świecie haskella"
```
## Głębsza wiedza

1. Historia: Nie ma precyzyjnej historii dla operacji "kapitalizacji stringów". Jest to jedna z pierwszych i najprostszych funkcji wprowadzanych do języków programowania.
2. Alternatywy: Możesz użyć wyzwalanej funcji `map`, ale to ma wpływ na każdą literę - nie tylko na pierwszą.
3. Szczegóły implementacyjne: Ważne jest, aby potraktować pusty string jako specjalny przypadek, aby uniknąć błędów.

## Zobacz też: 

1. [Hoogle](https://www.haskell.org/hoogle/): Wyszukiwarka do bibliotek Haskell.
2. [Oficjalna dokumentacja](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html): Szczegóły modułu `Data.Char`.
3. [Haskell Wiki](https://wiki.haskell.org/): Ogólne zasoby na temat Haskell.