---
title:                "Konwersja ciągu znaków na małe litery"
date:                  2024-01-20T17:38:38.275937-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konwersja ciągu znaków na małe litery"

category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Konwersja ciągu znaków na małe litery to zamiana wszystkich dużych liter w tekście na ich małe odpowiedniki. Programiści robią to aby ujednolicić dane, na przykład w celach porównywania tekstu czy obsługi haseł.

## How to: (Jak to zrobić?)
W Haskellu możesz użyć funkcji `toLower` z modułu `Data.Char`, aby przekształcić pojedynczy znak na małą literę. Aby przekształcić cały ciąg znaków, wykorzystaj funkcję `map`.

```haskell
import Data.Char (toLower)

convertToLower :: String -> String
convertToLower str = map toLower str

-- Przykład użycia:
main :: IO ()
main = putStrLn $ convertToLower "Hello, Haskell!"
```

Output:
```
hello, haskell!
```

## Deep Dive (Dogłębna analiza)
Konwersja na małe litery istniała od dawna w programowaniu. W językach obsługujących Unicode, jak Haskell, uwzględnia ona wiele skomplikowanych reguł związanych z różnymi systemami pisma. Alternatywą do `map toLower` może być użycie funkcji `toCaseFold`, która jest bliższa funkcjonalności `toLowerCase` z innych języków — obsługuje więcej przypadków niż same małe litery.

Implementacja `toLower` w Haskellu z modułu `Data.Char` korzysta z własności Unicode, aby przejrzeć wszelkie literowe odpowiedniki. Jest to ważne w kontekście międzynarodowym, gdzie tekst może zawierać znaki poza prostym alfabetem ASCII.

## See Also (Zobacz również)
- [Hackage: Data.Char](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Char.html) - dokumentacja modułu `Data.Char`.
- [Haskell Language Report: Textual Data](https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1240006) - specyfikacja języka Haskell dotycząca danych tekstowych.
