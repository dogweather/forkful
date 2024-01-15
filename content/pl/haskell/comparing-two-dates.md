---
title:                "Porównywanie dwóch dat."
html_title:           "Haskell: Porównywanie dwóch dat."
simple_title:         "Porównywanie dwóch dat."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w programowaniu potrzebujemy porównać dwa daty, na przykład aby upewnić się, że jedna jest późniejsza niż druga. Haskell ma wiele wbudowanych funkcji, które ułatwiają to zadanie. W tym artykule dowiesz się jak to zrobić.

## Jak To Zrobić

Porównywanie dat w Haskell jest proste, ponieważ operacje na datach są wykonywane za pomocą specjalnej stuktury danych o nazwie `Data.Time.UTCTime`. Aby wykonać porównanie, musimy najpierw utworzyć dwa obiekty `UTCTime`.

```Haskell
-- Tworzenie daty 1 stycznia 2021
let data1 = UTCTime (fromGregorian 2021 1 1) (secondsToDiffTime 0)
-- Tworzenie daty 31 grudnia 2020
let data2 = UTCTime (fromGregorian 2020 12 31) (secondsToDiffTime 0)
-- Porównanie czy data1 jest późniejsza niż data2
data1 > data2 -- zwróci wartość True
```

W powyższym przykładzie użyliśmy funkcji `fromGregorian`, która umożliwia tworzenie daty za pomocą rok, miesiąc i dzień. Funkcja `secondsToDiffTime` pozwala na ustawienie godziny i minut na 0. Możesz również użyć danych z prawdziwego życia, takich jak te pobrane z bazy danych lub wprowadzonych przez użytkownika.

## Głębszy Przegląd

Istnieje wiele funkcji, które możesz użyć do porównywania dat w Haskell. W funkcji `Data.Time.Clock` możesz znaleźć między innymi takie funkcje jak:
- `diffUTCTime` - oblicza różnicę pomiędzy dwiema datami w sekundach
- `addUTCTime` - dodaje określoną ilość sekund do podanej daty
- `getCurrentTime` - zwraca bieżącą datę i czas

Możesz również użyć funkcji z modułu `Data.Time.Calendar`, aby dokonać operacji na konkretnych częściach daty, takich jak dzień, miesiąc czy rok.

See Also

- [Dokumentacja modułu Data.Time.Clock](https://hackage.haskell.org/package/time/docs/Data-Time-Clock.html)
- [Dokumentacja modułu Data.Time.Calendar](https://hackage.haskell.org/package/time/docs/Data-Time-Calendar.html)