---
title:    "Haskell: Porównywanie dwóch dat"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dat jest częstym zadaniem w programowaniu, szczególnie gdy pracujemy z danymi, które są związane z czasem. W Haskellu istnieje wiele sposobów na porównywanie dat, a w tym artykule dowiesz się, jak to zrobić w sposób prosty i skuteczny.

## Jak to zrobić

Pierwszym krokiem jest zaimportowanie modułu `Data.Time`:

```Haskell
import Data.Time
```

Następnie, przyjrzyjmy się różnym sposobom na reprezentację daty w Haskellu. Możemy używać typu `Day`, który przechowuje datę w formacie rok-miesiąc-dzień, na przykład:

```Haskell
let date1 = fromGregorian 2020 12 01 :: Day
let date2 = fromGregorian 2020 11 30 :: Day
```

Możemy również używać typu `UTCTime`, który przechowuje datę i czas w formacie UTC, na przykład:

```Haskell
let dateTime1 = UTCTime (fromGregorian 2020 12 01) (secondsToDiffTime 0)
let dateTime2 = UTCTime (fromGregorian 2020 11 30) (secondsToDiffTime 0)
```

Aby porównać daty, możemy używać operatorów porównania, takich jak `<`, `<=`, `>`, `>=`. Na przykład, aby sprawdzić, czy `date1` jest wcześniejsze niż `date2`, możemy napisać:

```Haskell
date1 < date2 -- zwróci True
```

Możemy również użyć funkcji `compare`, która zwraca wartość typu `Ordering` (Less, Equal lub Greater) w zależności od wyniku porównania. Na przykład:

```Haskell
compare date1 date2 -- zwróci LT
```

Aby uzyskać różnicę w czasie między dwoma datami, możemy użyć funkcji `diffDays`. Na przykład:

```Haskell
diffDays date1 date2 -- zwróci -1
```

## Deep Dive

W Haskellu istnieje wiele funkcji i operacji, które możemy użyć do porównywania dat. Na przykład, możemy użyć funkcji `toGregorian` do konwersji daty `Day` na trójelementową tuplę z rokiem, miesiącem i dniem, na przykład:

```Haskell
toGregorian date1 -- zwróci (2020, 12, 01)
```

Możemy również użyć funkcji `diffUTCTime`, aby uzyskać różnicę w czasie między dwoma `UTCTime`. Na przykład:

```Haskell
diffUTCTime dateTime1 dateTime2 -- zwróci -86400 (w sekundach)
```

Inną przydatną funkcją jest `addDays`, która dodaje określoną liczbę dni do daty. Na przykład:

```Haskell
addDays 10 date1 -- zwróci 2020-12-11
```

W przypadku, gdy chcemy porównywać daty w różnych strefach czasowych, możemy użyć funkcji `zonedTimeToUTC`, aby przekonwertować datę w strefie czasowej na `UTCTime`, na przykład:

```Haskell
let zonedTime1 = ZonedTime (LocalTime (fromGregorian 2020 12 01) (TimeOfDay 12 0 0)) (hoursToTimeZone 1)
zonedTimeToUTC zonedTime1 -- zwróci UTC 2020-12-01 11:00:00
```

## Zobacz także

- Dokumentacja modułu Data.Time na stronie Hoogle: http://www.haskell.org/hoogle/?hoogle=Data.Time
- Przykładowe projekty wykorzystujące operacje na datach w Haskellu: https://github.com/topics/haskell-date-time