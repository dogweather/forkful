---
title:                "Ermittlung der Länge eines Strings"
html_title:           "Elm: Ermittlung der Länge eines Strings"
simple_title:         "Ermittlung der Länge eines Strings"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Finden der Länge einer Zeichenkette ist eine gängige Aufgabe bei der Programmierung. Dabei geht es darum, die Anzahl der Zeichen in einer Zeichenkette zu bestimmen. Programmierer tun dies, um beispielsweise die Größe eines Arrays zu bestimmen oder um die Eingabe des Benutzers zu überprüfen.

## So geht's:
Um die Länge einer Zeichenkette in Elm zu finden, kann die Funktion `String.length` verwendet werden. Diese Funktion erwartet eine Zeichenkette als Argument und gibt die Anzahl der Zeichen zurück. Hier ist ein Beispielcode:

```Elm
let
    string = "Hallo"
    length = String.length string
in
    length -- Ergebnis: 5
```

## Tiefgehende Informationen:
Die Idee, die Länge einer Zeichenkette zu bestimmen, ist nicht neu. Schon in den frühen Programmiersprachen wie C oder Java gab es entsprechende Funktionen. Alternativen zu `String.length` in Elm sind die Funktionen `String.foldl` und `String.foldr`, die jedoch etwas komplexer zu verwenden sind.

Intern verwendet die Funktion `String.length` in Elm eine Schleife, um jedes Zeichen in der Zeichenkette zu zählen. Dies ist eine effiziente Methode, die in den meisten Fällen ausreichend ist.

## Siehe auch:
- [Dokumentation zu String-Funktionen in Elm](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Umgang mit Zeichenketten in Elm](https://guide.elm-lang.org/strings/)