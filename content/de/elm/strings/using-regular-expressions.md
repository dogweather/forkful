---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:33.183553-07:00
description: "Regul\xE4re Ausdr\xFCcke (regex) in der Programmierung sind Muster,\
  \ die zum Abgleichen von Zeichenkombinationen in Strings verwendet werden. In Elm,\
  \ genau wie\u2026"
lastmod: '2024-02-25T18:49:50.858288-07:00'
model: gpt-4-0125-preview
summary: "Regul\xE4re Ausdr\xFCcke (regex) in der Programmierung sind Muster, die\
  \ zum Abgleichen von Zeichenkombinationen in Strings verwendet werden. In Elm, genau\
  \ wie\u2026"
title: "Regul\xE4re Ausdr\xFCcke verwenden"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke (regex) in der Programmierung sind Muster, die zum Abgleichen von Zeichenkombinationen in Strings verwendet werden. In Elm, genau wie in anderen Sprachen, verwenden Programmierer regex für Aufgaben wie das Validieren von Eingaben, das Suchen und Ersetzen von Text innerhalb von Strings aufgrund ihrer Flexibilität und Effizienz.

## Wie geht das:
Elm verfügt nicht über eingebaute regex-Funktionen in seiner Kernbibliothek, was die Verwendung von Drittanbieter-Bibliotheken für diese Operationen erforderlich macht. Eine der beliebten Auswahlmöglichkeiten für die Arbeit mit regex ist `elm/regex`. Sie können es Ihrem Projekt hinzufügen, indem Sie `elm install elm/regex` verwenden.

So können Sie `elm/regex` für einige häufige Aufgaben verwenden:

### 1. Ein Muster abgleichen
Um zu überprüfen, ob ein String einem Muster entspricht, können Sie `Regex.contains` verwenden.

```elm
import Regex

pattern : Regex.Regex
pattern = Regex.fromString "^[a-zA-Z0-9]+$" |> Maybe.withDefault Regex.never

isAlphanumeric : String -> Bool
isAlphanumeric input = Regex.contains pattern input

-- Beispiel Nutzung:
isAlphanumeric "Elm2023"     -- Ausgabe: True
isAlphanumeric "Elm 2023!"   -- Ausgabe: False
```

### 2. Alle Treffer finden
Um alle Vorkommen eines Musters in einem String zu finden, können Sie `Regex.find` verwenden.

```elm
matches : Regex.Regex
matches = Regex.fromString "\\b\\w+\\b" |> Maybe.withDefault Regex.never

getWords : String -> List String
getWords input = 
    input
        |> Regex.find matches
        |> List.map (.match)

-- Beispiel Nutzung:
getWords "Elm macht Spaß!"  -- Ausgabe: ["Elm", "macht", "Spaß"]
```

### 3. Text ersetzen
Um Teile eines Strings, die einem Muster entsprechen, zu ersetzen, verwenden Sie `Regex.replace`.

```elm
replacePattern : Regex.Regex
replacePattern = Regex.fromString "Elm" |> Maybe.withDefault Regex.never

replaceElmWithHaskell : String -> String
replaceElmWithHaskell input = 
    Regex.replace replacePattern (\_ -> "Haskell") input

-- Beispiel Nutzung:
replaceElmWithHaskell "Lernen mit Elm macht Spaß!"  
-- Ausgabe: "Lernen mit Haskell macht Spaß!"
```

In diesen Beispielen wird `Regex.fromString` verwendet, um ein regex-Muster zu kompilieren, wobei `\b` Wortgrenzen abgleicht und `\w` jedes Wortzeichen abgleicht. Behandeln Sie immer das `Maybe`-Ergebnis von `Regex.fromString`, um sich gegen ungültige regex-Muster abzusichern, typischerweise mit `Maybe.withDefault`.
