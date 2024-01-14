---
title:    "Haskell: Konwertowanie ciągu znaków na małe litery"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w programowaniu musimy zmienić wielkość liter w podanej przez użytkownika, lub odczytanej ze źródła zewnętrznego, ciągu znaków. Jest to ważne w celu porównywania tekstu lub wyświetlania go w jednolitym stylu. W języku Haskell istnieje wiele sposobów na dokonanie tej konwersji, a w tym artykule przyjrzymy się jednemu z nich.

## Jak to zrobić

Aby przekonwertować ciąg znaków na małe litery, musimy skorzystać z funkcji `map`, która przyjmuje funkcję i listę jako argumenty. W przypadku konwersji do małych liter, użyjemy funkcji `toLower` z modułu `Data.Char`. Poniżej znajduje się przykładowa funkcja, która wykonuje żądaną konwersję:

```Haskell
lowercase :: String -> String
lowercase str = map, toLower, str
```

Przykładowe wywołanie funkcji: `lowercase "HELLO WORLD"` zwróci `hello world`.

## Głębsza analiza

W języku Haskell funkcje są traktowane jako wartości, co oznacza, że mogą być przekazywane jako argumenty do innych funkcji. W przypadku funkcji `map` jako pierwszy argument przekazujemy funkcję, która zostanie wykonana na każdym elemencie podanej listy.

Funkcja `toLower` konwertuje pojedynczy znak na małą literę, a funkcja `map` wykonuje ją na każdym znaku w podanym ciągu. W ten sposób otrzymujemy listę z małymi literami, którą następnie łączymy przy użyciu funkcji `foldl` (lub `foldr`) w jedno słowo.

Innym sposobem na konwersję jest użycie funkcji `toLower` z modułu `Data.Text`, która ma wydajniejszą implementację dla ciągów znaków o dużej długości.

## Zobacz także

- [Dokumentacja języka Haskell](https://www.haskell.org/documentation/)
- [Poradnik dla początkujących w Haskellu](https://wiki.haskell.org/Haskell_for_beginners)
- [Moduł Data.Char](https://hackage.haskell.org/package/base/docs/Data-Char.html)
- [Porównywanie ciągów znaków w Haskellu](https://stackoverflow.com/questions/8513584/haskell-function-that-compare-two-string-and-return-a-value)