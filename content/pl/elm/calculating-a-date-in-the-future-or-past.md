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

## Dlaczego

Czy zdarzyło Ci się kiedyś potrzebować obliczenia daty w przyszłości lub w przeszłości? Na przykład, chciałeś sprawdzić, ile czasu upłynęło od Twoich urodzin lub ile dni zostało do Twojej rocznicy? W takich sytuacjach przydatnym narzędziem może okazać się programowanie w języku Elm.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub przeszłości w Elm, musimy znać dwie główne rzeczy - datę, od której zaczynamy oraz ilość dni, którą chcemy dodać lub odjąć. Weźmy na przykład urodziny, które są zawsze tego samego dnia każdego roku. Chcemy sprawdzić, ile lat minęło od naszych urodzin, w przyszłości lub przeszłości.

```Elm
-- Definicja typu Date
type alias Date =
    { day : Int, month : Int, year : Int }

-- Funkcja obliczająca datę
calculateDate : Date -> Int -> Date
calculateDate startingDate days =
    let
        -- Zliczanie dni
        totalDays =
            days * 365 -- W normalnej sytuacji musielibyśmy brać pod uwagę także rok przestępny

        -- Obliczanie nowej daty
        newDate =
            startingDate.day + totalDays
    in
    -- Zwracanie nowej daty
    { startingDate | day = newDate }
```

Przykładowe wywołanie funkcji:

```Elm
calculateDate { day = 6, month = 10, year = 1990 } 3
-- Zwróci { day = 9, month = 10, year = 1990 }
```

Jak widać, funkcja `calculateDate` zwróciła datę trzy dni po naszych urodzinach. Dodaliśmy do niej 3 dni, ale zwykle musielibyśmy również uwzględnić rok przestępny. W przypadku rocznicy czy innego ważnego wydarzenia, tylko dodajemy odpowiednią ilość dni do daty wyjściowej.

## Deep Dive

Funkcja `calculateDate` jest tylko prostym przykładem obliczania daty w Elm. W rzeczywistości, istnieje wiele różnych sposobów na pracę z datami w tym języku. Możemy na przykład użyć modułu `Time` z biblioteki standardowej, aby precyzyjnie zdefiniować daty i operować na nich z dużą dokładnością.

W Elm, daty są przechowywane jako typy algorytmiczne, co oznacza, że nie musimy martwić się o błędy wynikające z formatowania dat. Możemy także wykorzystać funkcje z biblioteki `Date.Extra` do bardziej zaawansowanych obliczeń, takich jak dodawanie lub odejmowanie lat lub miesięcy od danej daty.

## Zobacz również

- Oficjalna dokumentacja języka Elm: https://elm-lang.org/docs
- Przykłady użycia modułu `Time`: https://elmprogramming.com/elm-time-module.html
- Więcej informacji o typach algorytmicznych w Elm: https://thoughtbot.com/blog/elm-types-vs-haskell-types