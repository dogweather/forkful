---
title:                "Teilstrings extrahieren"
date:                  2024-01-20T17:45:34.068188-07:00
model:                 gpt-4-1106-preview
simple_title:         "Teilstrings extrahieren"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Substrings extrahieren bedeutet, spezifische Teile aus einem längeren String auszuwählen. Programmierer tun das, um Daten zu analysieren, zu manipulieren oder spezifische Informationen zu filtern.

## How to:
Hier sind ein paar Beispiele, um Substrings in Go zu extrahieren:

```go
package main

import (
	"fmt"
)

func main() {
	text := "Das ist ein Beispieltext"
	
	// Extrahieren mit Slicing
	substr1 := text[4:8]
	fmt.Println(substr1) // Ausgabe: "ist "
	
	// Extrahieren vom Anfang bis zu einem Punkt
	substr2 := text[:12]
	fmt.Println(substr2) // Ausgabe: "Das ist ein "
	
	// Extrahieren vom Punkt bis zum Ende
	substr3 := text[17:]
	fmt.Println(substr3) // Ausgabe: "Beispieltext"
}
```

## Deep Dive
Das Konzept, Teile eines Strings zu verwenden, gibt es seit den frühen Tagen der Programmierung. In Go wird das Slicing-Konzept vor allem durch die Einfachheit und Effizienz beim Speicherzugriff praktisch umgesetzt. Im Vergleich zu anderen Sprachen, wo manchmal Funktionen oder Methoden verwendet werden, zeichnet sich Go durch die einfache Syntax des Slicing aus. Wichtig zu wissen ist, dass das Slicing in Go in der Laufzeit O(1) operiert, weil es nur die Referenzpunkte des Originalstrings anpasst und keine neue Kopie erstellt.

## See Also
Weitere Details zur String-Verarbeitung in Go finden Sie hier:

- Offizielle Go-Dokumentation zu Strings: https://pkg.go.dev/strings
- Go Blog über Strings, Bytes, Runes und Characters: https://blog.golang.org/strings
- Tutorial zu Go Slices: https://blog.golang.org/slices-intro
