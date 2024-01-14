---
title:    "Haskell: Obliczanie daty w przyszłości lub w przeszłości"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego warto posiąść umiejętność obliczania daty w przyszłości lub przeszłości? Możliwość wyliczenia dokładnego dnia lub czasu w przyszłości jest niezwykle przydatna w wielu projektach programistycznych, ponieważ pozwala na planowanie działań lub wykonywanie skomplikowanych operacji z uwzględnieniem określonego dnia. 

## Jak to zrobić

Obliczanie daty w Haskellu jest bardzo proste dzięki wbudowanej bibliotece `Data.Time`. Aby ustawić datę w przyszłości lub przeszłości, wystarczy użyć funkcji `addDays` lub `addTime` wraz z odpowiednią ilością dni lub czasu, które chcemy dodać lub odjąć. 

```Haskell
import Data.Time

-- Obliczanie daty w przyszłości
futureDate = addDays 10 (fromGregorian 2021 12 15)  -- 25 grudnia 2021

-- Obliczanie daty w przeszłości
pastDate = addDays (-10) (fromGregorian 2021 12 15)  -- 5 grudnia 2021
```

## Głębsze zagłębienie

W przypadku bardziej skomplikowanych operacji z datami, warto poznać również bibliotekę `Data.Time.Calendar.OrdinalDate`, która pozwala na obliczanie daty na podstawie roku i numeru dnia w tym roku. Oprócz tego, wbudowana biblioteka `Data.Time.Clock` umożliwia pracę z czasem w formacie `UTCTime` oraz `LocalTime`, co może być przydatne w strefach czasowych. 

## Zobacz również

- Dokumentacja `Data.Time` : https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html
- Dokumentacja `Data.Time.Calendar.OrdinalDate` : https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Calendar-OrdinalDate.html
- Dokumentacja `Data.Time.Clock` : https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Clock.html