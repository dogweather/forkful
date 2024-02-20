---
date: 2024-01-20 17:32:59.236205-07:00
description: "Por\xF3wnywanie dat pozwala okre\u015Bli\u0107, kt\xF3ra z nich jest\
  \ wcze\u015Bniejsza, p\xF3\u017Aniejsza, czy mo\u017Ce identyczna. Programi\u015B\
  ci robi\u0105 to, \u017Ceby obs\u0142ugiwa\u0107 wydarzenia\u2026"
lastmod: 2024-02-19 22:04:54.599239
model: gpt-4-1106-preview
summary: "Por\xF3wnywanie dat pozwala okre\u015Bli\u0107, kt\xF3ra z nich jest wcze\u015B\
  niejsza, p\xF3\u017Aniejsza, czy mo\u017Ce identyczna. Programi\u015Bci robi\u0105\
  \ to, \u017Ceby obs\u0142ugiwa\u0107 wydarzenia\u2026"
title: "Por\xF3wnywanie dw\xF3ch dat"
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
