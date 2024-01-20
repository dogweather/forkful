---
title:                "Wydobywanie podciągów"
html_title:           "Python: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wyodrębnianie podciągów to proces selekcji specyficznego fragmentu ciągu tekstowego. Programiści robią to, aby manipulować danymi, przetwarzać tekst i analizować struktury danych.

## Jak to zrobić:

W Elm, mamy dostępną funkcję `String.slice`. Spróbujmy zobaczyć, jak to działa:

```Elm
import Html exposing (text)

main =
    text (extractSubstring 5 10 "Witaj, świecie Elm!")

extractSubstring : Int -> Int -> String -> String
extractSubstring start end str =
    String.slice start end str
```
Kiedy uruchomimy powyższy program, wynikiem będzie:
```Elm
"-- świec"
```

## W głąb tematu:

Historia: Elm, choć współczesny, jest mocno zainspirowany językami takimi jak Haskell i OCaml, które mają podobne funkcje do manipulowania ciągami.

Alternatywy: Innymi funkcjami, które mogą być użyteczne podczas pracy z ciągami w Elm są: `String.left`, `String.right` oraz `String.dropLeft` i `String.dropRight`.

Szczegóły implementacyjne: Funkcja `String.slice` w Elm działa w czasie liniowym, co oznacza, że czas potrzebny na jej wykonanie rośnie liniowo wraz z długością ciągu. Jest to związane z faktem, że Elm implementuje ciągi jako listy jednokierunkowe.

## Zobacz również:

- Oficjalna dokumentacja Elm do manipulacji ciągami: [https://package.elm-lang.org/packages/elm/core/latest/String](https://package.elm-lang.org/packages/elm/core/latest/String)
- Więcej o ciągach w Elm: [https://elmprogramming.com/strings.html](https://elmprogramming.com/strings.html)
- Poradnik Elm dla początkujących: [https://www.elm-tutorial.org/en/](https://www.elm-tutorial.org/en/)