---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Arduino: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die String-Interpolation ist ein Prozess, bei dem Werte in einen String eingefügt werden. Programmierer nutzen dies, um dynamische Strings zu erstellen und die Lesbarkeit des Codes zu verbessern.

## So geht's:
Hier ein Beispiel für die Verwendung von String-Interpolation in Go:
```Go
package main

import "fmt"

func main() {
  name := "World"
  fmt.Printf("Hello, %s!", name)
}
```
Wenn Sie das Programm ausführen, gibt es "Hello, World!" aus.

## Vertiefung
Im Go gab es vor Version 1.10 keine eingebaute Unterstützung für String-Interpolation. String-Formatierung mit 'fmt.Printf()' oder 'fmt.Sprintf()' wurde stattdessen verwendet.

Eine neuere Alternative in Go ist der `S`-Formatter in den verbreiteten Logrus und Log15 Paketen. Dieser ermöglicht benutzerdefinierte String-Interpolation in Logging-Aufrufen.

In Bezug auf die Implementierungsdetails verwendet Go's Printf-Funktion Reflexion, um herauszufinden, wie die Argumente aussehen, die ihm übergeben werden und sie dann entsprechend in den Ziel-String einzubinden.

## Siehe auch
- [Go Documentation on fmt](https://pkg.go.dev/fmt): Die offizielle Go-Dokumentation zur fmt-Bibliothek.
- [Go by Example: String Formatting](https://gobyexample.com/string-formatting): Ein praktisches Lernbeispiel für das Formatieren von Strings in Go.