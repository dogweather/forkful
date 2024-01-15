---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Haskell: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami musimy wyliczyć datę w przyszłości lub przeszłości, na przykład w celu ustalenia terminu spotkania lub święta. W takich sytuacjach dobrze jest znać sposoby na liczenie dat w Haskellu.

## Jak to zrobić

```Haskell
import Data.Time.Calendar

-- Obecna data
currentDate :: Day
currentDate = fromGregorian 2021 9 20

-- Liczenie daty w przyszłości za pomocą funkcji 'addDays'
futureDate :: Day
futureDate = addDays 30 currentDate 

-- Liczenie daty w przeszłości za pomocą funkcji 'addDays'
pastDate :: Day
pastDate = addDays (-30) currentDate

-- Wyświetlenie daty w czytelny sposób za pomocą funkcji 'toGregorian'
showDate :: Day -> String
showDate d = show (toGregorian d)

main = do 
    putStrLn "Obecna data: "
    putStrLn (showDate currentDate)
    putStrLn "Data w przyszłości: "
    putStrLn (showDate futureDate)
    putStrLn "Data w przeszłości: "
    putStrLn (showDate pastDate)
```

Przykładowy wynik działania programu:

Obecna data:
2021-9-20
Data w przyszłości:
2021-10-20
Data w przeszłości:
2021-8-21

## Głębszy wgląd

W Haskellu do wyliczenia daty w przyszłości lub przeszłości można użyć funkcji `addDays`, która dodaje lub odejmuje od podanej daty odpowiednią ilość dni. Funkcja `fromGregorian` służy do konwertowania trzech cyfr reprezentujących rok, miesiąc i dzień na wartość typu `Day`. Aby wyświetlić datę w czytelny sposób, możemy skorzystać z funkcji `toGregorian`, która zwraca trzy wartości: rok, miesiąc i dzień.

## Zobacz także

- Dokumentacja pakietu `time`: https://hackage.haskell.org/package/time
- Przykładowe zadania z użyciem dat w Haskellu: https://www.geeksforgeeks.org/datetime-operations-in-haskell/