---
title:                "Einsatz von regulären Ausdrücken"
date:                  2024-01-19
html_title:           "Bash: Einsatz von regulären Ausdrücken"
simple_title:         "Einsatz von regulären Ausdrücken"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke (Regex) sind Muster, die in Textstrings suchen und manipulieren. Entwickler nutzen sie, um effizient Daten zu validieren, zu extrahieren oder zu ersetzen.

## How to:
In Go verwendet man das `regexp` Package, um mit Regex zu arbeiten. Hier ein paar Beispiele:

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	// Ein einfaches Pattern finden
	re := regexp.MustCompile(`\w+`)
	fmt.Println(re.FindString("hello world")) // gibt "hello" aus

	// Alle Vorkommen finden
	reAll := regexp.MustCompile(`\w+`)
	fmt.Println(reAll.FindAllString("go gopher", -1)) // gibt ["go", "gopher"] aus

	// Strings ersetzen
	reReplace := regexp.MustCompile(`f\w+`)
	fmt.Println(reReplace.ReplaceAllString("foo fighter fight", "bar")) // gibt "bar barer bar" aus
}
```

## Deep Dive
Reguläre Ausdrücke gibt es seit den 1950er Jahren. In Go basieren sie auf Ken Thompsons 'grep' Konvention. Alternativen zu Regex sind String-Methoden wie `Contains`, `Index`, oder Parsing-Bibliotheken, falls Regex zu langsam oder zu kompliziert werden. Die Regex-Implementierung in Go ist rekursiv und kann nicht von evil Regex ausgenutzt werden, was in manchen anderen Sprachen zu Denial-of-Service führen kann.

## See Also
- Go Dokumentation zum `regexp` Package: [https://pkg.go.dev/regexp](https://pkg.go.dev/regexp)
- Wiki über reguläre Ausdrücke: [https://en.wikipedia.org/wiki/Regular_expression](https://en.wikipedia.org/wiki/Regular_expression)
- Online Regex-Testwerkzeuge wie [https://regex101.com/](https://regex101.com/) zum Experimentieren mit Regex-Patterns.
