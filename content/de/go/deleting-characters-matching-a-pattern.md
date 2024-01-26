---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
date:                  2024-01-20T17:42:06.369753-07:00
model:                 gpt-4-1106-preview
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?
Beim Löschen von Zeichen, die einem Muster entsprechen, geht's darum, spezifische Teile aus einem String herauszufiltern, die wir nicht brauchen oder wollen. Programmierer machen das oft, um Eingaben zu bereinigen oder Daten zu formatieren, bevor sie weiterverarbeitet werden.

## How to:
Ein Stückchen Go-Code sagt mehr als tausend Worte. Hier ein einfaches Beispiel wie das Ganze aussieht:

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	pattern := "[0-9]+"
	input := "Go Version 1.16 - Gophers unite!"

	// Kompiliere das Regexp-Muster
	re := regexp.MustCompile(pattern)

	// Ersetze alle Treffer mit einem leeren String
	cleaned := re.ReplaceAllString(input, "")

	// Ausgabe des bereinigten Strings
	fmt.Println(cleaned)
}
```

Ausgabe:

```
Go Version . - Gophers unite!
```

## Deep Dive
Reguläre Ausdrücke sind ein mächtiges Werkzeug, das es schon seit den 1950er Jahren gibt. In Go benutzen wir das `regexp` Paket, um damit umzugehen. Andere Möglichkeiten, Zeichen zu entfernen, wären Schleifen und Zeichen-Checks, aber wenn’s um Muster geht, sind reguläre Ausdrücke dein Freund. Implementierungsdetails, auf die du achten solltest, sind die Compilerung des Ausdrucks vor der Nutzung – das spart Zeit bei Mehrfachnutzung. Auch solltest du immer auf die Performance achten; Reguläre Ausdrücke können langsam sein bei großen Textmassen.

## See Also
Mehr Infos? Hier sind ein paar gute Anlaufstellen:

- Go’s `regexp` Paket Dokumentation: https://golang.org/pkg/regexp/
- Einführung in Reguläre Ausdrücke: https://www.regular-expressions.info/
- Go by Example: Regular Expressions: https://gobyexample.com/regular-expressions
