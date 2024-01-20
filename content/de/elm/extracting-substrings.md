---
title:                "Teilzeichenketten extrahieren"
html_title:           "PowerShell: Teilzeichenketten extrahieren"
simple_title:         "Teilzeichenketten extrahieren"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Extrahieren von Teilketten ist der Prozess, bestimmte Teile einer Zeichenkette zu isolieren. Programmierer tun dies, um relevante Daten aus einer größeren Textmasse zu extrahieren.

## So geht's:

Mit Elm kann man einen bestimmten Teil einer Zeichenkette extrahieren, indem man die eingebaute `String.slice`-Funktion verwendet. Hierfür musst du nur den Anfangs- und den Endindex des gewünschten Teilstrings angeben:

```Elm
import Html exposing (text)
import String 

extractSubstring start end str = 
    String.slice start end str

main = 
    text (extractSubstring 0 5 "Hallo Welt")
```

Die Ausgabe wäre in diesem Fall `"Hallo"`.

## Vertiefung

Historisch gesehen wurde das Konzept des Teilketten-Extrahierens schon früh in der Computerwissenschaft eingeführt und es hat sich seitdem in praktisch jeder Programmiersprache etabliert. In Elm ist die Implementierung sehr einfach und direkt, da das Extrahieren von Teilketten in der Sprache eingebaut ist und man keine Bibliotheken hinzufügen muss.

Diese Lösung ist für die meisten Anwendungen ausreichend. Es gibt jedoch alternative Methoden, wie die Verwendung der `String.left` oder `String.right` Funktion, die den Anfang oder das Ende einer Zeichenkette abhängig von der Anzahl der angegebenen Zeichen extrahieren.

## Siehe auch 

Für weitere Details zur Verwendung von `String.slice` und anderen String-Funktionen in Elm, schau dir die offizielle Dokumentation an: 
- [Elm String Dokumentation](https://package.elm-lang.org/packages/elm/core/latest/String)

Um mehr über die Geschichte von Zeichenketten und ihre Handhabung in der Informatik zu lernen, könnten diese Quellen interessant sein: