---
title:                "Zeichenketten verknüpfen"
date:                  2024-01-20T17:34:55.091585-07:00
model:                 gpt-4-1106-preview
simple_title:         "Zeichenketten verknüpfen"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?
String-Konkatenation ist das Verketten von Textteilen zu einem einzigen String. Wir nutzen das, um dynamische Texte zu erzeugen, Daten zu formatieren oder einfach während Debugging-Zwecken.

## So geht's:
```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	teil1 := "Hallo"
	teil2 := "Welt"
	// Einfache Verkettung mit dem Plus-Operator
	kombiniert := teil1 + ", " + teil2 + "!"
	fmt.Println(kombiniert) // "Hallo, Welt!"

	// Verkettung mit Join-Funktion aus dem strings-Paket für Arrays
	teile := []string{"Sprich", "Freund", "und", "tritt", "ein"}
	satz := strings.Join(teile, " ")
	fmt.Println(satz) // "Sprich Freund und tritt ein"
}
```

## Deep Dive
String-Konkatenation ist grundlegend und existiert, seitdem wir Programme schreiben. In Go ist es effizient, aber es gibt Einschränkungen. Wenn wir es mit vielen Strings zu tun haben, kann die Performance leiden. Hier kommt die `strings.Builder` Klasse ins Spiel, die eine mutable String-Bearbeitung ermöglicht. Sie ist effizienter, da sie nicht bei jeder Operation einen neuen String erzeugt.

Alternativen in anderen Sprachen, beispielsweise der `+` Operator in Java oder `.concat()` in JavaScript, haben ähnliche Performanzfragen, was zu Konzepten wie StringBuilders oder StringBuffers führte.

In Go sieht das so aus mit `strings.Builder`:
```Go
var builder strings.Builder
for _, value := range []string{"Go", "ist", "toll!"} {
	builder.WriteString(value)
	builder.WriteString(" ")
}
fmt.Println(builder.String().Trim()) // "Go ist toll!"
```

## Siehe auch
- Go Docs zu strings-Paket: https://pkg.go.dev/strings
- Blog über effiziente String-Konkatenation in Go: https://blog.golang.org/strings
- Go Wiki zu String-Operationen: https://github.com/golang/go/wiki/Strings
