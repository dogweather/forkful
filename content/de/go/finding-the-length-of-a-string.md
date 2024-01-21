---
title:                "Ermittlung der Zeichenkettenlänge"
date:                  2024-01-20T17:47:22.940884-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ermittlung der Zeichenkettenlänge"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Länge eines Strings zu ermitteln heißt, die Anzahl der Zeichen in diesem String zu zählen. Programmierer tun das, um Textdaten zu manipulieren und Größenbeschränkungen zu überprüfen.

## How to:

In Go gibt's die `len()`-Funktion, die direkt die Anzahl der Bytes eines Strings zurückgibt. Für UTF-8 Strings musst du `utf8.RuneCountInString()` nehmen. Hier zwei Beispiele:

```go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	asciiString := "Hallo Welt"
	unicodeString := "こんにちは"

	// Länge mit len() für einfache ASCII-Strings
	fmt.Println(len(asciiString))  // Ausgabe: 10

	// Länge mit utf8.RuneCountInString() für UTF-8
	fmt.Println(utf8.RuneCountInString(unicodeString))  // Ausgabe: 5
}
```

## Deep Dive

Ursprünglich zählte `len()` einfach die Bytes, was für ASCII-Texte reicht, da dort ein Zeichen gleich einem Byte ist. Doch UTF-8 hat unterschiedlich lange Zeichen (1-4 Bytes). `utf8.RuneCountInString()` löst dieses Problem, indem es durch die Runen läuft und zählt, was für internationale Anwendungen essentiell ist.

Alternativ zu diesen eingebauten Funktionen kannst du auch durch den String iterieren und manuell zählen, aber das ist weder elegant noch performant. Im Kontext von Unicode und Go ist es wichtig zu verstehen, dass "Zeichen" als "Rune" betrachtet werden, eine Variable, die ein Unicode Code Point repräsentiert.

## See Also:

- Go-Dokumentation über Strings: https://golang.org/pkg/strings/
- Go blog on strings: https://blog.golang.org/strings
- Unicode-Standard: http://unicode.org/standard/standard.html
- Go-Dokumentation über `unicode/utf8`-Paket: https://golang.org/pkg/unicode/utf8/