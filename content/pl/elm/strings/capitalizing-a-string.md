---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:33.304610-07:00
description: "Jak to zrobi\u0107: W Elm nie ma wbudowanej funkcji specjalnie do kapitalizacji\
  \ \u0142a\u0144cuch\xF3w znak\xF3w. Jednak mo\u017Cna \u0142atwo osi\u0105gn\u0105\
  \u0107 ten cel, korzystaj\u0105c z funkcji\u2026"
lastmod: '2024-03-13T22:44:35.304157-06:00'
model: gpt-4-0125-preview
summary: "W Elm nie ma wbudowanej funkcji specjalnie do kapitalizacji \u0142a\u0144\
  cuch\xF3w znak\xF3w."
title: "Zamiana liter na wielkie w \u0142a\u0144cuchu znak\xF3w"
weight: 2
---

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
