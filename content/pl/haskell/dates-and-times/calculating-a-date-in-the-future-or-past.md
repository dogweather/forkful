---
date: 2024-01-20 17:31:27.345461-07:00
description: "Obliczanie daty w przesz\u0142o\u015Bci lub przysz\u0142o\u015Bci to\
  \ zdefiniowanie momentu przed lub po danej dacie. Programi\u015Bci robi\u0105 to,\
  \ aby zarz\u0105dza\u0107 wydarzeniami,\u2026"
lastmod: '2024-03-13T22:44:35.466152-06:00'
model: gpt-4-1106-preview
summary: "Obliczanie daty w przesz\u0142o\u015Bci lub przysz\u0142o\u015Bci to zdefiniowanie\
  \ momentu przed lub po danej dacie. Programi\u015Bci robi\u0105 to, aby zarz\u0105\
  dza\u0107 wydarzeniami,\u2026"
title: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci"
weight: 26
---

## What & Why? | Co i dlaczego?
Obliczanie daty w przeszłości lub przyszłości to zdefiniowanie momentu przed lub po danej dacie. Programiści robią to, aby zarządzać wydarzeniami, terminami, planować zadania czy przewidywać daty ważnych wydarzeń.

## How to: | Jak to zrobić:
W Haskellu używamy biblioteki `time` do pracy z datami. Oto przykład dodawania dni do aktualnej daty:

```Haskell
import Data.Time

addDaysToCurrent :: Integer -> IO Day
addDaysToCurrent n = do
    today <- getCurrentTime
    let nDaysAfter = addDays n . utctDay $ today
    return nDaysAfter

main :: IO ()
main = do
    futureDate <- addDaysToCurrent 10
    print futureDate
```

Przykładowe wyjście:

```plaintext
2023-04-25
```

Aby obliczyć datę w przeszłości, odejmujemy dni:

```Haskell
subtractDaysFromCurrent :: Integer -> IO Day
subtractDaysFromCurrent n = do
    today <- getCurrentTime
    let nDaysBefore = addDays (-n) . utctDay $ today
    return nDaysBefore

main :: IO ()
main = do
    pastDate <- subtractDaysFromCurrent 10
    print pastDate
```

Przykładowe wyjście:

```plaintext
2023-04-05
```

## Deep Dive | Wnikliwa analiza:
Obliczanie dat w Haskellu wykorzystuje bibliotekę `time`, która jest standardem od momentu jej wprowadzenia w GHC 6.8. Jest ona inspirowana biblioteką Joda-Time z Javy. Alternatywy jak `old-time` są obecnie rzadziej używane.

Kluczowe typy danych to `UTCTime`, `Day`, `TimeOfDay`. Istnieje też spora gama funkcji jak `addDays` czy `diffUTCTime` dla operacji na datach.

Zarządzanie czasem jest niebanalne przez strefy czasowe i zmiany czasu (Daylight Saving Time). Biblioteka `time` radzi sobie z tymi zagadnieniami, lecz w praktyce brane są też inne biblioteki jak `timezone-series` czy `timezone-olson`.

## See Also | Zobacz również:
- Oficjalna dokumentacja biblioteki `time`: http://hackage.haskell.org/package/time
- Tutorial do zarządzania czasem w Haskellu: https://www.haskell.org/haskellwiki/Working_with_time
- Joda-Time, inspiracja dla `time`: https://www.joda.org/joda-time/
- Alternatywa `timezone-series`: http://hackage.haskell.org/package/timezone-series
