---
title:                "Zamiana liter na wielkie w ciągu znaków"
date:                  2024-01-19
html_title:           "Arduino: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"

category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)

Zmiana stringa tak, by zaczynał się z dużej litery, to "kapitalizacja". W Elmie robi się to, by dopasować tekst do konwencji językowych, jak np. tytuły czy imiona.

## How to: (Jak to zrobić:)

```Elm
module Capitalize exposing (capitalize)

capitalize : String -> String
capitalize string =
    if String.isEmpty string then
        string
    else
        let
            firstChar =
                String.left 1 string
                    |> String.toUpper

            remainingChars =
                String.dropLeft 1 string
        in
        firstChar ++ remainingChars

-- Przykład użycia i wynik
result = capitalize "cześć Elm!"
-- "Cześć Elm!"
```

## Deep Dive (Wgłębiając się)

Do Elm 0.19 nie było gotowej funkcji do kapitalizacji, więc trzeba było kombinować jak wyżej. Można też użyć pakietów zewnętrznych. Alternatywa to użycie `String.toUpper` na całym stringu, ale to nie to samo, co kapitalizacja.

Kapitalizacja to nie `toUpperCase`; nie chcemy wszystkich liter wielkich. Elm operuje na Unicodzie, co jest wygodne, ale uważaj na pułapki jak ligatury (np. "ﬂ" na "FL").

## See Also (Zobacz również)

- Elm `String` documentation: [Elm String](http://package.elm-lang.org/packages/elm/core/latest/String)
- Unicode issues with capitalization: [Unicode and Strings](https://unicode.org/reports/tr44/#General_Category_Values)
