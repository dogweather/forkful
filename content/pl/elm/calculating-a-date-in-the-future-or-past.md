---
title:                "Elm: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w programowaniu możemy być zmuszeni do wyznaczenia daty w przyszłości lub przeszłości na podstawie aktualnej daty. Jest to szczególnie przydatne, gdy projektujemy aplikacje związane z rezerwacją, planowaniem lub przypomnieniami. W tym wpisie dowiesz się, w jaki sposób można to łatwo osiągnąć w języku Elm.

## Jak to zrobić

Obliczanie daty w przyszłości lub przeszłości w języku Elm jest bardzo prostym zadaniem dzięki wbudowanym funkcjom. Aby uzyskać datę w przyszłości, możemy skorzystać z funkcji `add`, która przyjmuje dwa argumenty - ilość jednostek (np. dni, tygodni, miesięcy) oraz aktualną datę. Poniżej znajduje się przykładowy kod wykorzystujący tę funkcję:

```Elm
import Date exposing (..)

-- aktualna data
currentDate = Date.fromCalendarDate 2021 9 1

-- dodanie pięciu dni do daty
futureDate = Date.add Date.Day 5 currentDate

-- wypisanie daty w formacie RR-MM-DD
futureDateString = Date.format "%y-%m-%d" futureDate

-- wynik: 2021-09-06
```

Podobnie, aby uzyskać datę w przeszłości, możemy użyć funkcji `sub`:

```Elm
import Date exposing (..)

-- aktualna data
currentDate = Date.fromCalendarDate 2021 9 1

-- odjęcie pięciu dni od daty
pastDate = Date.sub Date.Day 5 currentDate

-- wypisanie daty w formacie RR-MM-DD
pastDateString = Date.format "%y-%m-%d" pastDate

-- wynik: 2021-08-27
```

## Głębszy zanurzenie

W Języku Elm można również obliczać daty biorąc pod uwagę różne strefy czasowe i dni tygodnia. Dokładny opis tych funkcji znajduje się w dokumentacji Elm. Jest to szczególnie przydatne, jeśli projektujemy aplikację dla użytkowników z różnych stref czasowych lub musimy uwzględnić dni wolne od pracy.

## Zobacz również

- Dokumentacja Elm dotycząca dat: https://package.elm-lang.org/packages/elm/time/latest/
- Przykładowe skrypty obliczające daty w przyszłości i przeszłości w języku Elm: https://github.com/elm-explorations/time/tree/master/examples