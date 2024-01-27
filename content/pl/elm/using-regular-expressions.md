---
title:                "Wykorzystanie wyrażeń regularnych"
date:                  2024-01-19
html_title:           "Arduino: Wykorzystanie wyrażeń regularnych"
simple_title:         "Wykorzystanie wyrażeń regularnych"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)

Regular expressions (regex) to wzorce służące do wyszukiwania i manipulowania tekstami. Programiści używają ich, by łatwo znajdować, weryfikować i zamieniać ciągi znaków przy minimalnym kodzie.

## How to: (Jak to zrobić:)

Elm używa pakietu `Regex` do pracy z wyrażeniami regularnymi. Poniżej kilka przykładów:

```Elm
import Regex exposing (..)

-- Sprawdzanie czy tekst pasuje do wzorca
isMatch : String -> String -> Bool
isMatch pattern text =
    case Regex.fromString pattern of
        Just re -> Regex.contains re text
        Nothing -> False

-- Użycie isMatch
isEmail : String -> Bool
isEmail email = isMatch "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" email

-- Wynik sprawdzenia
exampleEmailCheck : Bool
exampleEmailCheck = isEmail "example@test.com"
```

Przykład zwróci `True`, ponieważ string pasuje do wzorca emaila.

## Deep Dive (Więcej szczegółów)

Wyrażenia regularne mają korzenie w teorii automatów i języków formalnych z lat 50. Ich implementacje różnią się w zależności od języka, ale idea pozostaje taka sama. W Elm, regex obsługiwany jest przez pakiet `Regex`, ale jest mniej rozbudowany niż w językach takich jak JavaScript czy Python. Alternatywą może być stosowanie funkcji `String`, gdy potrzebujemy prostszej manipulacji tekstami.

## See Also (Zobacz też)

- Dokumentacja Elm `Regex`: https://package.elm-lang.org/packages/elm/regex/latest/Regex
- Interaktywny tester wyrażeń regularnych: https://regex101.com/
- Tutorial RegExp w JavaScript (dobrze tłumaczy podstawy, które są podobne): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions
