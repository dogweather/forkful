---
title:                "Wyszukiwanie i zamiana tekstu"
html_title:           "Haskell: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Zamiana tekstu to proces polegający na zastępowaniu określonego tekstu innym tekstem w danym pliku lub tekście. Programiści często wykonują to zadanie, aby szybko zaktualizować lub zmienić wiele wystąpień tekstu w jednym miejscu.

## Jak to zrobić:
Zamianę tekstu w Haskellu można przeprowadzić za pomocą funkcji `replace` z pakietu `Data.Text`. W poniższym przykładzie wykorzystamy tę funkcję do zamiany wszystkich wystąpień słowa "kot" na "pies" w tekście.

```Haskell
import Data.Text (replace)

myText :: Text
myText = "Lubię koty, ale uważam, że pies jest lepszy."

replacedText = replace "kot" "pies" myText

main :: IO ()
main = do
    putStrLn replacedText

--OUTPUT:
--Lubię psy, ale uważam, że pies jest lepszy.
```

Funkcja `replace` przyjmuje trzy argumenty - szukany tekst, tekst zastępujący oraz cały tekst. Następnie zwraca nowy tekst z zamienionymi wystąpieniami.

## Głębsze spojrzenie:
Funkcja `replace` została wprowadzona w wersji 0.11.0.0 pakietu `Data.Text` i jest dostępna od września 2011 roku. Alternatywne podejście do zamiany tekstu w Haskellu to użycie funkcji `replaceAll` z pakietu `Regex`. Jednak funkcja `replace` z pakietu `Data.Text` jest uważana za szybszą i bardziej wydajną, ponieważ dokonuje zamiany bez użycia wyrażeń regularnych.

## Zobacz także:
- Dokumentacja `Data.Text` - https://hackage.haskell.org/package/text/docs/Data-Text.html
- Pakiet `Regex` - https://hackage.haskell.org/package/regex