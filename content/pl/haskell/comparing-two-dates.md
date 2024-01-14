---
title:    "Haskell: Porównywanie dwóch dat"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat może być bardzo przydatne w programowaniu, ponieważ pozwala nam określić, która data jest wcześniejsza lub późniejsza. Może to mieć zastosowanie w wielu scenariuszach, takich jak tworzenie harmonogramów lub sortowanie danych chronologicznie.

## Jak to zrobić

W Haskellu porównywanie dwóch dat jest bardzo proste. Możemy użyć funkcji `compare`, która zwraca jeden z trzech wyników: `LT` (mniejsza), `EQ` (równa) lub `GT` (większa). Przyjrzyjmy się przykładowemu kodowi, który porównuje daty.

```Haskell
import Data.Time

--tworzymy dwa obiekty typu `Day`
data1 = fromGregorian 2021 01 01
data2 = fromGregorian 2020 01 01

--porównujemy je przy użyciu funkcji `compare`
comparison = compare data1 data2

--wypisujemy wynik porównania
print comparison
```

Wyjście powinno być `GT`, ponieważ `data1` jest późniejsza niż `data2`. Więcej informacji na temat typu `Day` i funkcji `compare` znajdziesz w sekcji "Deep Dive".

## Deep Dive

Typ `Day` jest częścią modułu `Data.Time`, który pozwala na manipulację i operacje na datach. `Day` jest typem prostym i zawiera tylko trzy wartości: rok, miesiąc i dzień, więc porównanie dwóch obiektów tego typu jest porównaniem pól tych dwóch obiektów. Funkcja `compare` jest częścią typu `Ord`, który jest odpowiedzialny za porównywanie typów prostych. Możemy również użyć operatora `>` lub `max` i `min` do porównania dat.

Pamiętaj, aby wykorzystać inne funkcje z modułu `Data.Time`, takie jak `getCurrentTime` czy `addDays`, aby dynamicznie tworzyć daty i dokonywać bardziej złożonych operacji.

## Zobacz również

- Dokumentacja modułu `Data.Time`: https://hackage.haskell.org/package/time/docs/Data-Time.html
- Przykładowe operacje na datach w Haskellu: https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/basic-date-and-time-types
- Porównywanie typów prostych w Haskellu: https://www.haskell.org/tutorial/numbers.html#sect11.7