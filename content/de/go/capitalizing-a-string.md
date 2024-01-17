---
title:                "Großschreibung eines Strings"
html_title:           "Go: Großschreibung eines Strings"
simple_title:         "Großschreibung eines Strings"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Capitalizing a string is the act of converting the first letter of each word in a string to uppercase. Programmers do this in order to improve the readability and aesthetics of their code, as well as to conform to certain coding conventions and style guides.

## Wie geht's:

Der folgende Code verwendet die ```Go strings``` Bibliothek, um eine Funktion zu erstellen, die einen gegebenen String in Großbuchstaben umwandelt und zurückgibt:

```
package main

import (
	"fmt"
	"strings"
)

func capitalize(s string) string {
	words := strings.Split(s, " ")
	for i, word := range words {
		words[i] = strings.Title(word)
	}
	return strings.Join(words, " ")
}

func main() {
	s := "hallo welt!"
	fmt.Println(capitalize(s))
	// Ausgabe: Hallo Welt!
}
```

## Ein tieferer Einblick:

Historisch gesehen war das Konzept des Großschreibens von Zeichen auf mechanischen Schreibmaschinen und Druckmaschinen aus technischen Gründen notwendig. Heutzutage gibt es auch alternative Methoden, um einen String zu capitalizen, wie z.B. durch die Verwendung von regulären Ausdrücken oder benutzerdefinierten Funktionen. Die Implementierung der capitalization-Funktion kann je nach Programmiersprache und verwendeter Methode variieren.

## Siehe auch:

- https://blog.golang.org/strings
- https://www.geeksforgeeks.org/golang-strings-title-function/
- https://gobyexample.com/uppercase-strings