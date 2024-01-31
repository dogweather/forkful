---
title:                "Suchen und Ersetzen von Text"
date:                  2024-01-20T17:57:42.014480-07:00
model:                 gpt-4-1106-preview
simple_title:         "Suchen und Ersetzen von Text"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?
Textsuche und -ersatz ermöglichen das Auffinden und Modifizieren von Textstrings in Daten. Programmierer verwenden es, um Code zu korrigieren, Daten zu manipulieren oder Textmuster schnell zu ändern.

## How to:
Mit dem `strings` Paket in Go kannst du ganz einfach Text suchen und ersetzen. Hier ist ein kurzes Beispiel:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	originalText := "Hallo Welt! Hallo Programmierung!"
	search := "Hallo"
	replaceWith := "Guten Tag"
	
	result := strings.ReplaceAll(originalText, search, replaceWith)
	fmt.Println(result)
}
```

Ausgabe:

```
Guten Tag Welt! Guten Tag Programmierung!
```

## Deep Dive:
Die Funktion `strings.ReplaceAll` wurde in Go 1.12 eingeführt und löste die ältere `strings.Replace` Funktion mit einem `-1` als vierten Argument ab, um alle Instanzen zu ersetzen. Alternativen beinhalten Reguläre Ausdrücke (Regex) für komplexere Suchmuster, die man mit `regexp` Paket in Go umsetzen kann. Bei größeren Textmengen kann die Effizienz des Textersatzes wichtig sein; beachte, dass `strings.ReplaceAll` direkten Text ersetzt, während Regex-Ersetzungen mehr Rechenleistung erfordern.

## See Also:
- Go Dokumentation zum `strings` Paket: https://pkg.go.dev/strings
- Go by Example mit `strings.Replace`: https://gobyexample.com/string-functions
- Reguläre Ausdrücke in Go: https://pkg.go.dev/regexp
