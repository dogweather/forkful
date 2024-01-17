---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Elm: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Konwersja daty na ciąg znaków jest procesem przekształcania informacji o dacie w postaci liczb na czytelny dla człowieka format. Programiści często wykonują taką operację, aby wyświetlić datę w czytelnej formie dla użytkowników lub do zapisania jej w odpowiednim formacie w bazie danych.

## Jak?

```Elm
import Date exposing (toUtc, format)
import Time exposing (fromPosix, Hours, posixToMillis)

timeInHours : Int
timeInHours = Hours 24

timestamp : Int
timestamp =
    posixToMillis (fromPosix timeInHours)

formatDate : String
formatDate =
    format "dd MMMM yyyy" (toUtc timestamp)

-- output: "12 października 2021"
```

## Wnikliwe omówienie

(1) Przed wprowadzeniem standardu Unix Epoch w 1970 roku, różne systemy operacyjne i języki programowania używały różnych formatów daty, co utrudniało interoperacyjność. (2) Alternatywą do konwertowania daty na ciąg znaków jest przechowywanie jej jako liczbę i wykonanie obliczeń na tej liczbie. (3) W Elm, funkcja `toUtc` konwertuje datę na czas UTC, który jest uniwersalnym standardem, a `format` pozwala na wybór wyświetlanego formatu daty.

## Zobacz też

- Oficjalna dokumentacja Elm dotycząca pracy z datami: https://guide.elm-lang.org/dates_and_times/
- Alternatywny sposób konwersji daty w Elm: https://package.elm-lang.org/packages/elm/time/latest/Time#mod
- Porównanie różnych formatów daty w różnych językach programowania: https://en.wikipedia.org/wiki/Date_format_by_country