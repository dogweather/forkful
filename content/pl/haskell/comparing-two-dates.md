---
title:                "Porównywanie dwóch dat"
html_title:           "Haskell: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Porównywanie dwóch dat jest procesem porównywania dwóch dat zapisanych w formacie daty lub czasu, aby określić, która z nich jest późniejsza lub wcześniejsza. Programiści często wykonują to w celu sortowania lub filtrowania danych w aplikacjach.

## Jak to zrobić:

```Haskell
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Calendar.OrdinalDate (show, toGregorian)

-- Przykładowe daty:
let date1 = fromGregorian 2021 9 10
let date2 = fromGregorian 2021 9 12

-- Porównanie dwóch dat:
date1 < date2 -- Zwraca True

-- Konwersja daty na string w formacie "rok-miesiąc-dzień":
show $ toGregorian date1 -- Zwraca "2021-9-10"
```

## Głębszy Zanurzenie:

Porównywanie dat jest powszechne od dawna i nie jest ograniczone do jednego języka programowania. Można to zrobić również w innych językach, takich jak Java czy Python, używając odpowiednich funkcji. W języku Haskell funkcje związane z porównywaniem dat znajdują się w bibliotece Data.Time.Calendar.

## Zobacz również:

Dokumentacja Haskell: https://www.haskell.org/hoogle/?hoogle=date