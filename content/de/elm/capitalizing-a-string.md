---
title:                "Ein String großschreiben"
html_title:           "Elm: Ein String großschreiben"
simple_title:         "Ein String großschreiben"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Wir alle wissen, wie wichtig es ist, auf die Kleinschreibung zu achten, insbesondere beim Schreiben von E-Mails oder Textnachrichten. Aber wussten Sie, dass auch Programmierer auf die Groß- und Kleinschreibung achten müssen, wenn sie mit Strings arbeiten? Es geht darum, sicherzustellen, dass bestimmte Teile des Codes richtig ausgeführt werden und keine Fehler verursachen. Daher ist das Kapitalisieren von Strings eine wichtige Aufgabe für Programmierer.

## So geht's:
Die Funktion `String.toUpper` in Elm ermöglicht es uns, einen String komplett in Großbuchstaben umzuwandeln. Hier ist ein Beispiel, wie wir es verwenden können:

```
Elm String.toUpper "hallo" -- Output: "HALLO"
```

Wenn wir jedoch nur den ersten Buchstaben groß schreiben möchten, können wir die Funktion `String.capitalize` verwenden:

```
Elm String.capitalize "guten tag" -- Output: "Guten tag"
```

## Tiefes Eintauchen:
Historisch gesehen spielte die Groß- und Kleinschreibung eine wichtige Rolle bei der Programmierung, da frühe Computer dies als wichtigen Unterschied erkannten. Heutzutage können moderne Sprachen wie Elm dies jedoch automatisch erkennen und daher ist das Kapitalisieren von Strings keine kritische Aufgabe mehr. Alternativ können Programmierer auch reguläre Ausdrücke verwenden, um bestimmte Zeichenfolgen zu identifizieren und zu bearbeiten. Die Implementierung von `String.toUpper` in Elm basiert auf dem Unicode-Standard, der eine internationale Standardisierung von Zeichen ermöglicht.

## Siehe auch:
- [Dokumentation zu `String.toUpper` in Elm](https://package.elm-lang.org/packages/elm-lang/core/5.1.1/String#toUpper)
- [Das Unicode-Konsortium](https://unicode.org/index.html)