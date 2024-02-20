---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:33.304610-07:00
description: "Kapitalizacja \u0142a\u0144cucha znak\xF3w polega na przekszta\u0142\
  ceniu pierwszej litery danego \u0142a\u0144cucha na wielk\u0105 liter\u0119, zachowuj\u0105\
  c reszt\u0119 liter w formie ma\u0142ych, cz\u0119sto\u2026"
lastmod: 2024-02-19 22:04:54.433696
model: gpt-4-0125-preview
summary: "Kapitalizacja \u0142a\u0144cucha znak\xF3w polega na przekszta\u0142ceniu\
  \ pierwszej litery danego \u0142a\u0144cucha na wielk\u0105 liter\u0119, zachowuj\u0105\
  c reszt\u0119 liter w formie ma\u0142ych, cz\u0119sto\u2026"
title: "Zamiana liter na wielkie w \u0142a\u0144cuchu znak\xF3w"
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
