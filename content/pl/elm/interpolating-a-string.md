---
title:                "Interpolacja łańcuchów znaków"
date:                  2024-01-20T17:50:38.903551-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolacja łańcuchów znaków"

category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Interpolacja stringów pozwala nam wplatać wartości zmiennych w ciągi tekstowe, ułatwiając dynamiczne tworzenie treści. Programiści używają interpolacji, by kod był bardziej elastyczny i czytelny.

## How to:
W Elm nie ma bezpośredniego mechanizmu interpolacji stringów, jak w innych językach. Używamy funkcji `String.concat` lub operatora `++`, by osiągnąć podobny efekt.

```Elm
name = "Alicja"
greeting = "Cześć, " ++ name ++ "!"

-- Rezultat: "Cześć, Alicja!"
```

Możesz też użyć funkcji `String.concat` dla listy części:

```Elm
name = "Alicja"
age = "25"

greeting = String.concat ["Cześć, ", name, "! Masz ", age, " lata."]

-- Rezultat: "Cześć, Alicja! Masz 25 lata."
```

## Deep Dive
Elm kładzie duży nacisk na czytelność i bezpieczeństwo typów, co tłumaczy brak klasycznej interpolacji stringów. W przeszłości, programiści polegali na szablonach lub konkatenacji, aby "wplatać" dane do stringów. W Elm, musimy zawsze jawnie przekształcić wartości na stringi (używając na przykład `String.fromInt` dla intów) zanim połączymy je z innymi stringami.

Oto alternatywy:

- Funkcje pomocnicze, które DBają o konwersję i łączenie.
- Funkcje formatujące, takie jak printf w innych językach, choć wymagają one pakietów zewnętrznych w Elm, np. `elm-format-string`.

Przykład funkcji pomocniczej:

```Elm
helloPerson : String -> Int -> String
helloPerson name age =
    "Cześć, " ++ name ++ "! Masz " ++ String.fromInt(age) ++ " lata."

-- Użycie funkcji:
greeting = helloPerson "Alicja" 25

-- Rezultat: "Cześć, Alicja! Masz 25 lata."
```

Interpolacja stringów w innych językach, jak JavaScript czy Python, jest bardziej bezpośrednia, ale Elm zachowuje prostotę dzięki unikaniu dodatkowej składni.

## See Also
- Elm `String` moduł: https://package.elm-lang.org/packages/elm/core/latest/String
- Pakiet `elm-format-string` do bardziej zaawansowanego formatowania: https://package.elm-lang.org/packages/lukewestby/elm-string-interpolate/latest/
- Wprowadzenie do Elm (PL): https://elm-lang.org/docs
