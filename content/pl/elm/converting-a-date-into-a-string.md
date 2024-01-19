---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Clojure: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego?

Konwersja daty na ciąg w Elm polega na przekształceniu wartości daty w konkretną sekwencję znaków. Programiści robią to, aby móc wyświetlać daty w czytelnej dla użytkownika formie, albo zapisywać je w formie tekstowej np. w bazie danych.

## Jak to zrobić:

W Elm możemy to zrealizować za pomocą pakietu `justinmimbs/date`. Przykładowy kod przedstawiający konwersję daty na ciąg znaków prezentuje się tak:

```Elm
import Date exposing (fromIsoString, toIsoStringInZone, utc)

convertDate : Maybe String -> Maybe String
convertDate isoDate =
    isoDate
        |> Maybe.andThen fromIsoString
        |> Maybe.map (toIsoStringInZone utc)
```

Działanie tego kodu to zwrócenie daty jako ciągu znaków w formacie ISO 8601.

## Pogłębiona analiza

Pierwotnie w Elm nie było wbudowanej funkcji umożliwiającej proste przekształcenie daty do ciągu znaków. Dopiero popularność języka i rozwijające się społeczności dookoła niego przyczyniły się do stworzenia mnóstwa praktycznych pakietów, takich jak `justinmimbs/date`.

Alternatywą dla tej metody może być wykorzystanie pakietu `elm/bytes`, który umożliwia bardziej niskopoziomowe manipulowanie danymi, ale jest też trudniejszy w obsłudze.

Detal implementacyjny: pakiet `justinmimbs/date` konwertuje daty w taki sposób, że zwraca ciągi znaków w formacie UTC, co gwarantuje unikalność konkretnego momentu na różnych strefach czasowych.

## Zobacz też

Więcej informacji na temat używanego tutaj pakietu do manipulacji datami znajdziesz tutaj: 
- Dokumentacja pakietu `justinmimbs/date`: https://package.elm-lang.org/packages/justinmimbs/date/latest/
- Elm Bytes, zaawansowane manipulacje danymi: https://package.elm-lang.org/packages/elm/bytes/latest/