---
date: 2024-01-20 17:57:30.337747-07:00
description: 'So geht''s: In Elm benutzt du oft Funktionen aus dem `String`-Modul,
  um Text zu manipulieren. Hier ist ein einfaches Beispiel.'
lastmod: '2024-03-13T22:44:53.789623-06:00'
model: gpt-4-1106-preview
summary: In Elm benutzt du oft Funktionen aus dem `String`-Modul, um Text zu manipulieren.
title: Suchen und Ersetzen von Text
weight: 10
---

## So geht's:
In Elm benutzt du oft Funktionen aus dem `String`-Modul, um Text zu manipulieren. Hier ist ein einfaches Beispiel:

```elm
import String

searchAndReplace : String -> String -> String -> String
searchAndReplace searchTerm replacement text =
    String.replace searchTerm replacement text

main =
    let
        originalText = "Hallo Welt!"
        newText = searchAndReplace "Welt" "Elm" originalText
    in
    -- Output: "Hallo Elm!"
    text newText
```

## Tiefere Einblicke:
Historisch ist Suchen und Ersetzen ein fundamentaler Bestandteil der Texteditoren und wurde bereits in den frühesten EDV-Systemen verwendet. In Elm, wie in vielen funktionalen Sprachen, ist es wichtig, unveränderliche Datenstrukturen zu beachten – das ersetzen von Text erzeugt also immer einen neuen String. Alternativen in anderen Sprachen können ähnliche Methoden verwenden oder auf reguläre Ausdrücke zurückgreifen, um komplexere Muster zu suchen und zu ersetzen. Die Implementation in Elm ist aufgrund seiner einfachen Syntax und seines Fokus auf Zuverlässigkeit besonders unkompliziert und fehlerresistent gestaltet.

## Siehe auch:
- Elm `String` Dokumentation: https://package.elm-lang.org/packages/elm/core/latest/String#replace
- Regex-Unterstützung in Elm mit `elm/regex`: https://package.elm-lang.org/packages/elm/regex/latest
- Ein thread zu Suchen und Ersetzen auf Elm Discourse: https://discourse.elm-lang.org/
