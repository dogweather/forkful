---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Elm: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Jak i czemu liczymy daty do przodu i do tyłu w Elm?

## Co i dlaczego?
Obliczanie daty w przyszłości lub przeszłości to operacja polegająca na dodaniu albo odjęciu określonej ilości dni od podstawowej daty. Programiści robią to, gdy potrzebują zapytania o datę umowy, przewidywanego czasu dostawy, okresu ważności lub inne powiązane zdarzenia przyszłości.


## Jak to zrobić:
Podstawowa biblioteka daty Elm zawiera funkcję `Date.add`. Oto krótki przykład:

```Elm
module Main exposing (..)

import Date
import Time

futureDate : Date.Date
futureDate =
    let
        today = Date.fromTime <| Time.millisToPosix 0
    in
    Date.add Date.year 1 today
```
Ten fragment kodu oblicza datę za rok od początku ery Unix (1 stycznia 1970). Wynik będzie: `1971-01-01`.

## Wgłębienie:
Obliczanie daty w przyszłości lub przeszłości w Elm odbywa się poprzez dodawanie lub odejmowanie określonych okresów. Podstawowa biblioteka daty Elm dostarcza nam cztery różne okresy: dzień, miesiąc, rok i kwartał.

Alternatywą dla wbudowanej biblioteki jest pakiet `justinmimbs/date-extra`, który zapewnia dodatkową elastyczność i precyzję, pozwalając dodać lub odjąć dowolną ilość minut, godzin itp.

Podczas implementowania takiego kodu, istotne jest pamiętanie, że daty są niemutowalne. Oznacza to, że nie możemy "zmienić" daty dodając do niej dni. Zamiast tego, tworzymy nową datę bazując na poprzedniej.

## Zobacz także:
1. Elm Date Docs: https://package.elm-lang.org/packages/elm/time/latest/
2. Date Extra Package: https://package.elm-lang.org/packages/justinmimbs/date-extra/latest/
3. Elm Time Docs: https://package.elm-lang.org/packages/elm/time/latest/Time