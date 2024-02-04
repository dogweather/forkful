---
title:                "Zamiana liter na wielkie w łańcuchu znaków"
date:                  2024-02-03T19:05:33.304610-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zamiana liter na wielkie w łańcuchu znaków"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Kapitalizacja łańcucha znaków polega na przekształceniu pierwszej litery danego łańcucha na wielką literę, zachowując resztę liter w formie małych, często w celu ujednolicenia formatowania lub czytelności. Programiści często wykonują to zadanie, aby zapewnić spójne prezentowanie danych, zwłaszcza w interfejsach użytkownika lub podczas przetwarzania i wyświetlania wprowadzonych przez użytkownika danych.

## Jak to zrobić:

W Elm nie ma wbudowanej funkcji specjalnie do kapitalizacji łańcuchów znaków. Jednak można łatwo osiągnąć ten cel, korzystając z funkcji wbudowanego modułu `String`, takich jak `toUpper`, `toLower`, `left` i `dropLeft`.

```elm
capitalize : String -> String
capitalize str =
    if String.isEmpty str then
        ""
    else
        String.toUpper (String.left 1 str) ++ String.toLower (String.dropLeft 1 str)

-- Przykład użycia
main =
    String.toList "hello world" |> List.map capitalize |> String.join " "
    -- Wynik: "Hello World"
```

Dla bardziej złożonych scenariuszy lub jeśli preferujesz użycie biblioteki, która zapewnia bezpośrednią możliwość kapitalizacji łańcuchów znaków, możesz rozważyć pakiet zewnętrzny, tak jak `elm-community/string-extra`. Jednakże, według mojej ostatniej aktualizacji, ekosystem Elm zachęca do radzenia sobie z tego typu zadaniami, korzystając z funkcji wbudowanych, aby zachować język i projekty w prostocie.

```elm
import String.Extra as StringExtra

-- W przypadku, gdy w bibliotece zewnętrznej znajduje się funkcja `capitalize`
capitalizeWithLibrary : String -> String
capitalizeWithLibrary str =
    StringExtra.capitalize str

-- Przykład użycia z hipotetyczną funkcją biblioteczną
main =
    "this is elm" |> capitalizeWithLibrary
    -- Hipotetyczny wynik: "This is elm"
```

Zawsze sprawdzaj repozytorium pakietów Elm, aby uzyskać najnowsze i najbardziej preferowane biblioteki do manipulacji łańcuchami znaków, jeśli szukasz dodatkowej funkcjonalności poza standardową biblioteką.
