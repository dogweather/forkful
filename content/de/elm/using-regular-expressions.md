---
title:                "Verwendung regulärer Ausdrücke"
html_title:           "Elm: Verwendung regulärer Ausdrücke"
simple_title:         "Verwendung regulärer Ausdrücke"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?

Reguläre Ausdrücke ermöglichen es uns, in Texten nach bestimmten Mustern zu suchen und diese zu manipulieren. Programmierer verwenden sie, um komplexe Aufgaben wie das Validieren von Eingaben oder das Durchsuchen von Dateien schnell und präzise zu erledigen.

## Wie geht's:

Das Grundgerüst eines regulären Ausdrucks in Elm ist das Paket "Regex". Es enthält Funktionen wie "Regex.match", "Regex.replace" und "Regex.split". Zum Beispiel können wir mit "Regex.match" nach bestimmten Zeichenfolgen suchen, mit "Regex.replace" diese Zeichenfolgen in einem Text manipulieren und mit "Regex.split" einen Text in abschnittsweisen Teile teilen.

```Elm
import Regex

-- Sucht nach dem Wort "Hallo" in einem Text
Regex.match "Hallo" "Hallo, wie geht es dir?" -- gibt "True" zurück

-- Ersetzt das Wort "heißt" mit "ist" in einem Text
Regex.replace "heißt" "Mein Name ist Jake" "Mein Name heißt Jake" -- gibt "Mein Name ist Jake" zurück

-- Teilt einen Text an jedem Leerzeichen
Regex.split " " "Eins Zwei Drei Vier" -- gibt ["Eins", "Zwei", "Drei", "Vier"] zurück
```

## Tiefes Eintauchen:

Reguläre Ausdrücke wurden erstmals in den 1950er Jahren von Mathematikern und Linguisten entwickelt. Sie haben seitdem viele Anwendungen gefunden, einschließlich der Verwendung in Programmiersprachen wie Elm. Es gibt auch alternative Methoden, um nach Mustern in Texten zu suchen, wie zum Beispiel die Verwendung von "String.contains" oder "String.split". Die Implementierung von regulären Ausdrücken in Elm verwendet eine sogenannte "Regular Expression Engine", die aus mehreren Komponenten besteht und die Regeln des regulären Ausdrucks interpretiert.

## Siehe auch:

Weitere Informationen zu regulären Ausdrücken in Elm finden Sie in der offiziellen Dokumentation unter https://elm-lang.org/docs/regular-expressions. Sie können auch auf der Seite des Regex-Pakets auf GitHub nach zusätzlichen Funktionen suchen: https://package.elm-lang.org/packages/elm/regex/latest/.