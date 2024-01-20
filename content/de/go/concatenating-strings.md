---
title:                "Strings verketten"
html_title:           "Bash: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?

String-Konkatenation ist der Prozess, bei dem zwei oder mehr Strings zu einem einzigen String verbunden werden. Programmierer machen das, um komplexe Nachrichten zu erstellen oder Daten in Textform zu verarbeiten.

## Wie man:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str1 := "Hallo"
	str2 := ", Welt!"

	// String-Konkatenation
	result := str1 + str2

	fmt.Println(result)  // Ausgabe: Hallo, Welt!

	// Einsatz von strings.Join
	resultJoin := strings.Join([]string{str1, str2}, "")
	fmt.Println(resultJoin)  // Ausgabe: Hallo, Welt!
}
```

## Vertiefung:

Die Konkatenation von Strings ist nicht neu und geht auf die Anfänge der Programmierung zurück. Eine Alternative zur Verwendung des "+"-Operators in Go ist die Funktion `strings.Join()`, die effizienter ist, wenn mehrere Strings verkettet werden. Intern werden in Go keine neuen Zeichenketten erstellt, wenn Sie zwei Zeichenketten concat mithilfe des "+"-Operators. Stattdessen werden die Zeichenketten verknüpft und auf die gleiche Speicheradresse verwiesen.

## Siehe auch:

- Go Dokumentation über Strings: https://golang.org/pkg/strings/
- Artikel über die Effizienz von `strings.Join()`: https://stackoverflow.com/questions/1760757/how-to-efficiently-concatenate-strings-in-go