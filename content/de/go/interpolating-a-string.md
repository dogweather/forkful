---
title:                "Zeichenketten interpolieren"
date:                  2024-01-20T17:50:54.484580-07:00
model:                 gpt-4-1106-preview
simple_title:         "Zeichenketten interpolieren"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
String-Interpolation ermöglicht es, Variablenwerte in festen Text einzubetten. Das vereinfacht die dynamische Erzeugung von Nachrichten und die Formatierung von Ausgaben.

## So geht's:
```Go
package main

import (
	"fmt"
)

func main() {
	name := "Friedrich"
	age := 42

	// String Interpolation mit Printf
	fmt.Printf("Hallo, mein Name ist %s und ich bin %d Jahre alt.\n", name, age)

	// String Interpolation mit Sprintf (zur Speicherung in einer Variablen)
	message := fmt.Sprintf("Hallo, mein Name ist %s und ich bin %d Jahre alt.", name, age)
	fmt.Println(message)
}
```
Ausgabe:
```
Hallo, mein Name ist Friedrich und ich bin 42 Jahre alt.
Hallo, mein Name ist Friedrich und ich bin 42 Jahre alt.
```

## Vertiefung
In Go ist der Begriff "String-Interpolation" eigentlich eine Fehlbezeichnung; richtig arbeitet man mit der "Formatierung", die in der `fmt`-Bibliothek verwaltet wird. Historisch gesehen kommt die Art der String-Formatierung aus C, wo die `printf`-Funktion diese ermöglichte. In Go gibt es keine echte Interpolation wie in anderen Sprachen, etwa mit `"${variable}"` – stattdessen benutzt man Platzhalter wie `%s` für Strings und `%d` für Integer. Der Vorteil liegt in der strengeren Typisierung und Formatkontrolle. Alternativen zur `fmt`-Bibliothek wären das direkte Zusammensetzen von Strings mit `+` oder der Einsatz von `strings.Builder` für komplexe Situationen.

## Siehe auch
- Go’s fmt-Paket Dokumentation: https://pkg.go.dev/fmt
- "Effective Go" zur Formatierung: https://golang.org/doc/effective_go#printing
