---
title:                "Porównywanie dwóch dat"
aliases:
- /pl/haskell/comparing-two-dates/
date:                  2024-01-20T17:32:59.236205-07:00
model:                 gpt-4-1106-preview
simple_title:         "Porównywanie dwóch dat"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
Porównywanie dat pozwala określić, która z nich jest wcześniejsza, późniejsza, czy może identyczna. Programiści robią to, żeby obsługiwać wydarzenia chronologicznie, zarządzać terminami i sprawdzać okresy ważności.

## How to:
Do porównania dat w Haskellu możemy użyć modułu `Data.Time`. Oto przykład:

```haskell
import Data.Time

compareDates :: IO ()
compareDates = do
    let date1 = fromGregorian 2023 3 15 -- 15 marzec 2023
    let date2 = fromGregorian 2023 4 1 -- 1 kwiecień 2023
    print $ compare date1 date2 -- LT
    print $ date1 < date2 -- True
    print $ date1 == date2 -- False
    print $ date1 > date2 -- False

main :: IO ()
main = compareDates
```

## Deep Dive
Porównywanie dat to klasyczne zadanie. W Haskellu, `Data.Time` jest standardowym modułem do zarządzania czasem, który dostarcza m.in. typ `Day` dla dat bez czasu. Alternatywnie, możemy użyć `Data.Calendar` lub zewnętrzne biblioteki jak `time` czy `chronos`. Ważne jest, że `Day` ma instancję klasy `Ord`, co umożliwia używanie `<`, `>` czy `==` do porównywania.

## See Also
- [Haskell Data.Time library](https://hackage.haskell.org/package/time-1.11.1.1/docs/Data-Time.html)
- [Haskell Hierarchical Libraries](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/)
- [SO - Comparing Dates in Haskell](https://stackoverflow.com/questions/36068284/comparing-dates-in-haskell)
