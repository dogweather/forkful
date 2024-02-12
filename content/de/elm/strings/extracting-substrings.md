---
title:                "Teilstrings extrahieren"
aliases:
- de/elm/extracting-substrings.md
date:                  2024-01-20T17:45:35.858143-07:00
model:                 gpt-4-1106-preview
simple_title:         "Teilstrings extrahieren"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
(## Was & Warum?)
Extrahieren von Teilzeichenketten bedeutet, spezifische Teile aus einem längeren String herauszunehmen. Programmierer machen das, um mit Daten zu jonglieren, Nutzereingaben zu verarbeiten oder einfach spezifische Informationen aus einem Text zu filtern.

## How to:
(## Anleitung:)
Elm bietet verschiedene Funktionen, um mit Strings zu arbeiten. Hier ein paar Beispiele, wie man Teilzeichenketten extrahiert:

```Elm
import String

-- Extrahiere einen Teil eines Strings von einer Startposition bis zum Ende
substringStartToEnd : String -> String
substringStartToEnd text =
    String.slice 5 (String.length text) text
  
-- Extrahiere einen Teil eines Strings von einer Startposition bis zu einer Endposition
substringStartToEndPos : String -> String
substringStartToEndPos text =
    String.slice 0 5 text

-- Nutzen der Funktionen
main =
    let
        originalText = "Elm ist großartig!"
        part1 = substringStartToEnd originalText       -- "ist großartig!"
        part2 = substringStartToEndPos originalText    -- "Elm i"
    in
    -- Hier könnte die Logik zum Anzeigen von `part1` und `part2` folgen, z.B. in einer HTML-View
```

## Deep Dive:
(## Hintergründe:)
Die Funktion `String.slice` in Elm ist ähnlich zu JavaScript's `substring` und hat ihre Wurzeln in den Anfängen der stringverarbeitenden Vorgängerprogrammiersprachen. Es ist zuverlässig, aber Achtung: Elm ist 0-basiert, d.h. der Startindex fürs Extrahieren ist 0, nicht 1. Alternativen wie `String.left` und `String.right` bieten noch andere Wege, um an Anfang oder Ende eines Strings zu schneiden, ohne die genauen Indizes zu benötigen.

Ein wichtiger Punkt ist auch die Performance – bei großen Strings kann das Extrahieren von Teilstrings aufgrund der internen Repräsentation von Strings in Elm und anderen Sprachen aufwändiger werden. Immer daran denken, das nur bei Bedarf zu machen.

## See Also:
(## Siehe auch:)
- Elm's `String` Modul Dokumentation: https://package.elm-lang.org/packages/elm/core/latest/String
- Elm Programmierleitfäden: https://guide.elm-lang.org/
- Weiterführende Artikel zur Stringmanipulation in funktionalen Sprachen.
