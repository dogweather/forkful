---
aliases:
- /pl/elm/using-regular-expressions/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:44.072939-07:00
description: "Wyra\u017Cenia regularne (regex) w programowaniu to wzorce u\u017Cywane\
  \ do dopasowywania kombinacji znak\xF3w w ci\u0105gach znak\xF3w. W Elm, podobnie\
  \ jak w innych j\u0119zykach,\u2026"
lastmod: 2024-02-18 23:08:49.509353
model: gpt-4-0125-preview
summary: "Wyra\u017Cenia regularne (regex) w programowaniu to wzorce u\u017Cywane\
  \ do dopasowywania kombinacji znak\xF3w w ci\u0105gach znak\xF3w. W Elm, podobnie\
  \ jak w innych j\u0119zykach,\u2026"
title: "Korzystanie z wyra\u017Ce\u0144 regularnych"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wyrażenia regularne (regex) w programowaniu to wzorce używane do dopasowywania kombinacji znaków w ciągach znaków. W Elm, podobnie jak w innych językach, programiści używają regexów do zadań takich jak walidacja danych wejściowych, wyszukiwanie oraz zamiana tekstu w ciągach znaków ze względu na ich elastyczność i wydajność.

## Jak to zrobić:
Elm nie posiada wbudowanych funkcji regex w swojej głównej bibliotece, wymaga używania bibliotek stron trzecich dla tych operacji. Jednym z popularnych wyborów do pracy z regexami jest `elm/regex`. Możesz dodać to do swojego projektu używając `elm install elm/regex`.

Oto jak możesz używać `elm/regex` do kilku częstych zadań:

### 1. Dopasowywanie wzorca
Aby sprawdzić, czy ciąg znaków pasuje do wzorca, możesz użyć `Regex.contains`.

```elm
import Regex

pattern : Regex.Regex
pattern = Regex.fromString "^[a-zA-Z0-9]+$" |> Maybe.withDefault Regex.never

isAlphanumeric : String -> Bool
isAlphanumeric input = Regex.contains pattern input

-- Przykład użycia:
isAlphanumeric "Elm2023"     -- Wyjście: True
isAlphanumeric "Elm 2023!"   -- Wyjście: False
```

### 2. Znajdowanie wszystkich dopasowań
Aby znaleźć wszystkie wystąpienia wzorca w ciągu znaków, możesz użyć `Regex.find`.

```elm
matches : Regex.Regex
matches = Regex.fromString "\\b\\w+\\b" |> Maybe.withDefault Regex.never

getWords : String -> List String
getWords input = 
    input
        |> Regex.find matches
        |> List.map (.match)

-- Przykład użycia:
getWords "Elm is fun!"  -- Wyjście: ["Elm", "is", "fun"]
```

### 3. Zamiana tekstu
Aby zamienić części ciągu znaków, które pasują do wzorca, użyj `Regex.replace`.

```elm
replacePattern : Regex.Regex
replacePattern = Regex.fromString "Elm" |> Maybe.withDefault Regex.never

replaceElmWithHaskell : String -> String
replaceElmWithHaskell input = 
    Regex.replace replacePattern (\_ -> "Haskell") input

-- Przykład użycia:
replaceElmWithHaskell "Ucząc się Elm jest zabawnie!"  
-- Wyjście: "Ucząc się Haskell jest zabawnie!"
```

W tych przykładach użycie `Regex.fromString` służy do kompilacji wzorca regex, gdzie `\b` pasuje do granic słów, a `\w` do dowolnego znaku słowa. Zawsze obsługuj wynik `Maybe` z `Regex.fromString` dla ochrony przed nieprawidłowymi wzorcami regex, typowo używając `Maybe.withDefault`.
