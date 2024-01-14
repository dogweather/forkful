---
title:                "Haskell: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Często spotykamy się z sytuacją, gdy musimy porównać dwie daty w naszym programie. Może to być potrzebne, aby upewnić się, że użytkownik podaje prawidłowe dane, lub w celu sortowania lub filtrowania danych. W tym artykule dowiesz się, jak porównywać daty w języku Haskell i jak to zrobić w najlepszy sposób.

## Jak to zrobić

Język Haskell oferuje nam kilka sposobów na porównywanie dat. Jednym z nich jest użycie funkcji `compare`, która porównuje dwie daty i zwraca wartość `Ordering`. Istnieją trzy możliwe wartości `Ordering` -`LT`, `GT` i `EQ`, które odpowiadają odpowiednio mniejszej, większej lub równej wartości. Poniżej znajduje się przykład kodu, który pokazuje to w praktyce:

```Haskell
import Data.Time

date1 = fromGregorian 2019 1 1
date2 = fromGregorian 2020 1 1

main = do
  let comparison = compare date1 date2
  putStrLn $ "Porównanie dat: " ++ show comparison
```

Output:
```
Porównanie dat: LT
```

Możemy również użyć funkcji `==`, `<=` lub `>=`, aby bezpośrednio porównać dwie daty. Przykład:

```Haskell
date1 == date2 -- False
date1 <= date2 -- True
```

Możemy również użyć biblioteki `time` w celu porównania dat, tak jak w przykładzie poniżej:

```Haskell
import Data.Time

date1 = fromGregorian 2019 1 1
date2 = fromGregorian 2020 1 1

main = do
  let diffDays = diffDays date1 date2
  putStrLn $ "Liczba dni między datami: " ++ show diffDays
```

Output:
```
Liczba dni między datami: -365
```

## Głębszy wgląd

W języku Haskell, daty są reprezentowane za pomocą typu `Day`, który jest częścią biblioteki `time`. Typ ten przechowuje tylko datę, bez informacji o czasie. W bibliotece `time` możemy również używać typów `UTCTime` lub `LocalTime`, jeśli potrzebujemy również danych o czasie.

Kiedy porównujemy daty, ważne jest, aby być ostrożnym ze strefami czasowymi. Na przykład, jeśli chcemy porównać daty z różnych stref, musimy konwertować je do odpowiedniej strefy czasowej przed porównaniem. W przeciwnym razie, porównanie może nie być dokładne.

## Zobacz też

- Dokumentacja biblioteki `time`: https://hackage.haskell.org/package/time
- Porównywanie wartości w języku Haskell: https://www.haskell.org/tutorial/comparison.html.