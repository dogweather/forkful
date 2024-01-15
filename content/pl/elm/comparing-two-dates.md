---
title:                "**Porównywanie dwóch dat"
html_title:           "Elm: **Porównywanie dwóch dat"
simple_title:         "**Porównywanie dwóch dat"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Często potrzebujemy porównać dwie daty w naszych programach, na przykład aby ustalić która jest wcześniejsza lub bardziej aktualna. W tych przypadkach przydatne jest zapoznanie się z funkcją porównującą daty w języku Elm.

## Jak to zrobić

```Elm
import Date

Date.compare (Date.fromCalendarDate 2021 01 01) (Date.fromCalendarDate 2021 04 15) 
-- Zwraca następującą wartość: LT
```

Funkcja `compare` przyjmuje dwa argumenty - obiekt `Date` dla pierwszej i drugiej daty. W powyższym przykładzie porównujemy datę 1 stycznia 2021 roku z datą 15 kwietnia 2021 roku. Wynik wyrażony jest za pomocą trzech możliwych wartości: `LT` - jeśli pierwsza data jest wcześniejsza, `GT` - jeśli pierwsza data jest późniejsza lub `EQ` - jeśli obie daty są sobie równe.

Możemy również skorzystać z funkcji `compare` w warunkowych instrukcjach za pomocą operatorów `==`, `<`, `<=`, `>`, `>=`. Na przykład:

```Elm
Date.compare (Date.fromCalendarDate 2021 01 01) (Date.fromCalendarDate 2021 01 15) == LT 
-- Zwraca wartość True
```

## Wnikliwa analiza

Funkcja `compare` wykorzystuje standardową metodę porównywania dat - najpierw są sprawdzane lata, potem miesiące, a na końcu dni. Dzięki temu możemy bez problemu porównywać daty z różnymi formatami, na przykład z datami zawierającymi tylko rok i miesiąc.

Funkcja `compare` może również przyjmować obiekty `Time` jako argumenty, dzięki czemu możemy porównywać daty i czasy jednocześnie. Funkcja ta jest również bezpieczna - jeśli jako argumenty podamy niepoprawne daty, zwróci ona wartość `Error`.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o funkcji `compare` i porównywaniu dat w języku Elm, polecamy zapoznanie się z dokumentacją oraz wypróbowanie różnych przykładów z poradnika: 
- https://package.elm-lang.org/packages/elm/time/latest/Time#compare
- https://guide.elm-lang.org/dates_and_times.html#comparing-dates

Dziękujemy za lekturę! W razie pytań zapraszamy do komentowania lub zadawania ich na forum społeczności Elm.