---
title:                "Untersuchen von Teilzeichenketten"
html_title:           "Elm: Untersuchen von Teilzeichenketten"
simple_title:         "Untersuchen von Teilzeichenketten"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Substring-Extraktion ist ein gängiger Vorgang in der Programmierung, bei dem ein Teil eines Strings oder einer Zeichenkette aus einem größeren String extrahiert wird. Programmierer nutzen dies häufig, um bestimmte Daten oder Informationen aus Strings zu isolieren und zu verwenden.

## So geht's:
```Elm
import String

-- Beispiel: Extrahieren des Wortes "Blau" aus einem Satz
String.dropUntil "Blau" "Der Himmel ist Blau und die Sonne scheint"

-- Output: "Blau und die Sonne scheint"
```

```Elm
import String

-- Beispiel: Extrahieren aller Vokale aus einem String
String.filter (\c -> List.member (String.toLower c) ["a", "e", "i", "o", "u"]) "Hallo Welt"

-- Output: "a ae"
```

## Tiefentauchen:
Die erste Implementierung von Substring-Extraktion wurde in der Programmiersprache SNOBOL (StriNg Oriented and symBOlic Language) entwickelt, die 1960 von David Farber, Ralph Griswold und Ivan Polonsky entworfen wurde. Heutzutage gibt es auch alternative Methoden, um Substrings zu extrahieren, wie zum Beispiel reguläre Ausdrücke. In Elm kann der Befehl `String.access` verwendet werden, um auf bestimmte Zeichen innerhalb eines Strings zuzugreifen und sie auszugeben.

## Siehe auch:
https://package.elm-lang.org/packages/elm/strings/latest/String#right
https://www.tutorialspoint.com/snobol_programming/snobol_language_basics.htm
https://developer.mozilla.org/de/docs/Web/JavaScript/Guide/Regular_Expressions