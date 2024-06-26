---
date: 2024-01-20 17:42:06.648614-07:00
description: "Vorgehensweise: In Elm gibt es keine eingebaute RegEx-Bibliothek wie\
  \ in anderen Sprachen. Zum Musterabgleich und L\xF6schen von Zeichen k\xF6nnen wir\
  \ mit\u2026"
lastmod: '2024-03-13T22:44:53.788591-06:00'
model: gpt-4-1106-preview
summary: In Elm gibt es keine eingebaute RegEx-Bibliothek wie in anderen Sprachen.
title: "L\xF6schen von Zeichen, die einem Muster entsprechen"
weight: 5
---

## Vorgehensweise:
In Elm gibt es keine eingebaute RegEx-Bibliothek wie in anderen Sprachen. Zum Musterabgleich und Löschen von Zeichen können wir mit `String`-Funktionen arbeiten oder externe Pakete nutzen.

Hier ein einfaches Beispiel, um Ziffern aus einem String zu entfernen:

```Elm
import String

removeDigits : String -> String
removeDigits text =
    String.filter (\char -> not (Char.isDigit char)) text

main =
    String.filter (\char -> not (Char.isDigit char)) "Elm12345"
    -- Ergebnis: "Elm"
```

Falls komplexere Muster benötigt werden, kann das paket `elm/regex` verwendet werden:

```Elm
import Regex

removePattern : String -> String -> String
removePattern pattern text =
    Regex.replace Regex.All (Regex.regex pattern) (\_ -> "") text

main = 
    removePattern "[0-9]+" "Elm12345"
    -- Ergebnis: "Elm"
```

## Tiefere Einblicke:
Historisch gesehen hatten funktionale Sprachen wie Elm weniger Fokus auf reguläre Ausdrücke, weil sie oft schwer zu lesen und zu warten sind. Stattdessen ermutigt Elm zur Nutzung von präziseren String-Funktionen. Mit `elm/regex` bietet die Sprache jedoch auch die Möglichkeit, komplexere Musterabfragen durchzuführen, wenn benötigt. Alternative Methoden zum Löschen von Zeichen aus Strings könnten benutzerdefinierte Parser oder das Durchlaufen des Strings mit Zustandslogik sein.

## Siehe auch:
- Elm `String` Dokumentation: [https://package.elm-lang.org/packages/elm/core/latest/String](https://package.elm-lang.org/packages/elm/core/latest/String)
- `elm/regex` Paket: [https://package.elm-lang.org/packages/elm/regex/latest](https://package.elm-lang.org/packages/elm/regex/latest)
