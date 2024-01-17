---
title:                "Extrahieren von Teilstrings"
html_title:           "Go: Extrahieren von Teilstrings"
simple_title:         "Extrahieren von Teilstrings"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Extrahieren von Teilstrings ist eine häufige Aufgabe in der Programmierung. Dabei geht es darum, Teilbereiche eines Strings, also einer Zeichenkette, aus einem größeren String zu entfernen oder herauszunehmen. Programmierer nutzen diese Funktion, um zum Beispiel Daten aus komplexen Strings auszulesen oder um einen String in mehrere Teile aufzuteilen.

## So geht's:
Um Teilstrings in Go zu extrahieren, können wir die Funktion `strings` aus der Standardbibliothek verwenden. Hier ein Beispielcode:
```Go
package main

import "fmt"
import "strings"

func main() {
  sentence := "Einen schönen Tag noch"
  substring := strings.Split(sentence, " ")[1]
  fmt.Println(substring)
}
```
Dieser Code gibt das Wort "schönen" aus, da wir mit `Split` den Satz anhand der Leerzeichen in einzelne Wörter aufteilen und dann das zweite Wort auswählen.

## Tiefer in die Materie:
Die Konzept des Teilstring-Extrahierens existiert schon seit langem in der Programmierung und wurde auch in älteren Programmiersprachen wie C oder Java verwendet. Neben der `strings`-Funktion gibt es in Go auch die Möglichkeit, reguläre Ausdrücke für das Extrahieren von Teilstrings zu verwenden. Alternativ können auch spezielle Bibliotheken wie `strconv` genutzt werden, um Zahlen aus einem String auszulesen.

## Siehe auch:
- [Go Strings Package Documentation](https://golang.org/pkg/strings/)
- [A Tour of Go - Strings](https://tour.golang.org/basics/5)
- [Go Regular Expressions Tutorial](https://www.w3schools.com/go/go_regular_expressions.asp)