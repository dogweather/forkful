---
date: 2024-01-20 17:38:08.077721-07:00
description: "Ein String in Kleinbuchstaben umzuwandeln bedeutet, alle Zeichen des\
  \ Strings in ihre Kleinbuchstaben-\xC4quivalente zu transformieren. Programmierer\
  \ nutzen\u2026"
lastmod: '2024-03-13T22:44:53.791467-06:00'
model: gpt-4-1106-preview
summary: "Ein String in Kleinbuchstaben umzuwandeln bedeutet, alle Zeichen des Strings\
  \ in ihre Kleinbuchstaben-\xC4quivalente zu transformieren."
title: Umformung eines Strings in Kleinbuchstaben
weight: 4
---

## How to:
In Elm kannst du den `String.toLower`-Funktion nutzen, um einen String in Kleinbuchstaben umzuwandeln. So geht's:

```Elm
import String

lowercaseString : String -> String
lowercaseString str =
    String.toLower str

-- Beispiel-Nutzung
main =
    lowercaseString "Hallo, Welt!"  --> "hallo, welt!"
```

Der Aufruf `lowercaseString "Hallo, Welt!"` gibt dir `"hallo, welt!"` zurück.

## Deep Dive
Die Umwandlung zu Kleinbuchstaben ist ein klassisches Problem und ein Standardfeature in vielen Programmiersprachen. Historisch gesehen folgt Elm hier anderen funktionalen Sprachen wie Haskell oder ML. Elm vereinfacht dies im Vergleich zu JavaScript, wo unterschiedliche Browser unterschiedlich auf `String.toLowerCase()`-Aufrufe reagieren könnten.

Es gibt Alternativen, wie zum Beispiel die manuelle Umwandlung jedes Zeichens durch eine eigens definierte Funktion, was aber meist unnötig komplex ist. Die Implementierung von `String.toLower` nutzt die Unicode-Standardisierung, um zuverlässige Ergebnisse zu garantieren, egal für welche Sprache oder welches Zeichensystem.

## See Also
Zum Vertiefen empfehle ich folgende Ressourcen:

- Elm's offizielle `String`-Dokumentation: [package.elm-lang.org/packages/elm/core/latest/String#toLower](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- Unicode-Standard: [unicode.org/reports/tr21/tr21-5.html](https://unicode.org/reports/tr21/tr21-5.html)
