---
title:    "Go: Zusammenfügen von Zeichenketten"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Warum

In der Programmierung ist es oft notwendig, Strings miteinander zu verbinden, um Daten oder Texte zu manipulieren oder auszugeben. In Go gibt es mehrere Möglichkeiten, eine effektive String-Konkatenation zu erreichen.

## Wie man es macht

```Go
package main 

import (
	"fmt"
)

func main() {
	// Verwendung des "+" Operators 
	firstName := "Max"
	lastName := "Mustermann"
	fullName := firstName + " " + lastName
	fmt.Println(fullName) // Ausgabe: Max Mustermann

	// Verwendung der "fmt.Sprintf" Funktion
	age := 25
	info := fmt.Sprintf("Name: %s, Alter: %d", fullName, age)
	fmt.Println(info) // Ausgabe: Name: Max Mustermann, Alter: 25
}
```

## Tiefer eintauchen

In Go sind Strings unveränderlich, was bedeutet, dass sie nicht direkt manipuliert werden können. Stattdessen werden bei der Konkatenation neue Strings erstellt und die alten verworfen. Dies kann zu Leistungsproblemen führen, wenn große Datenmengen verarbeitet werden müssen. Um dies zu vermeiden, empfiehlt es sich, den "strings.Builder" zu verwenden, der speziell für die effiziente Konkatenation von Strings entwickelt wurde.

## Siehe auch

- [https://golang.org/pkg/bytes/#Buffer](https://golang.org/pkg/bytes/#Buffer)
- [https://golang.org/pkg/strings/](https://golang.org/pkg/strings/)