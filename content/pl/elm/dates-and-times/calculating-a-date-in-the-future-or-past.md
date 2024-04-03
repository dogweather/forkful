---
date: 2024-01-20 17:30:44.293435-07:00
description: "Jak to zrobi\u0107: ."
lastmod: '2024-03-13T22:44:35.335319-06:00'
model: gpt-4-1106-preview
summary: .
title: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci"
weight: 26
---

## Jak to zrobić:
```Elm
import Time
import Date

-- Obliczanie daty 10 dni w przyszłości:
futureDate : Date.Date -> Date.Date
futureDate date =
    Date.add Date.Day 10 date

-- Obliczanie daty 5 lat wstecz:
pastDate : Date.Date -> Date.Date
pastDate date =
    Date.add Date.Year -5 date

-- Przykład:
today = Date.fromTime (Time.millisToPosix 1622505600000) -- Przyjmujemy, że dziś jest 1 czerwca 2021

tomorrow = futureDate today
-- Date.fromTime (Time.millisToPosix 1622592000000) --> 11 czerwca 2021

fiveYearsAgo = pastDate today
-- Date.fromTime (Time.millisToPosix 1464825600000) --> 2 czerwca 2016
```

## Głębsze spojrzenie
Obliczanie daty w przyszłości lub przeszłości nie jest nową potrzebą – znajduje się w historii technologii od momentu, gdy systemy zaczęły wykorzystywać cykle czasowe. W Elm, podobnie jak w innych językach, istnieje wbudowana biblioteka `Date`, która ułatwia takie działania. Alternatywy obejmują użycie zewnętrznych pakietów, jak `elm-time-travel`, które mogą oferować więcej funkcji. W implementacji kluczowe jest dokładne rozumienie, jak biblioteka obchodzi się z przestępnymi sekundami, strefami czasowymi i innymi niuansami związanymi z czasem.

## Zobacz również
- [Oficjalna dokumentacja Elm dla pakietu Date](https://package.elm-lang.org/packages/elm/time/latest/)
- [ISO 8601 na Wikipedia](https://pl.wikipedia.org/wiki/ISO_8601) – standard formatowania i obliczania czasu używany w wielu technologiach, w tym w Elm.
