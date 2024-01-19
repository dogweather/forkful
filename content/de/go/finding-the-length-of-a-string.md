---
title:                "Die Länge eines Strings ermitteln"
html_title:           "Java: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Ermitteln der Länge eines Strings ist eine grundlegende Operation, die die Anzahl der Zeichen in einer Zeichenkette ermittelt. Programmierer benötigen diese Information oft, um die Kontrolle über Schleifen zu behalten, Arrays sicher zu indizieren und Text effizient zu verarbeiten.

## So geht's:

In Go wird die Länge eines Strings mit der eingebauten Funktion `len()` gefunden. Hier ein kurzer Codeausschnitt:

```Go
package main

import "fmt"

func main() {
	str := "Hallo, Welt!"
	fmt.Println(len(str))
}
```

Dieser Code produziert die Ausgabe `13`, da der String "Hallo, Welt!" dreizehn Zeichen enthält.

## Hintergrundinfo:

**Historischer Kontext**: Das Konzept der Längenmessung von Zeichenketten ist so alt wie die Computerprogrammierung selbst. Ein-Byte-Zeichen und ASCII-Tabellen bildeten das klassische Modell.

**Alternativen**: In manchen Sprachen, etwa in JavaScript, verwenden Sie `.length`. Go hält es einfach und konsistent - Sie verwenden `len()` für Arrays, Slices und Strings.

**Implementierungsdetails**: Go verwendet UTF-8 für die String-Codierung. Daher gibt `len()` die Anzahl der Bytes, *nicht* der Zeichen, zurück. Zum Zählen der Unicode-Zeichen benötigen Sie die `utf8.RuneCountInString()` Funktion.

```Go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	str := "Hallo, 世界!"
	fmt.Println(len(str))                    // Gibt "14" zurück, die Anzahl der Bytes
	fmt.Println(utf8.RuneCountInString(str)) // Gibt "10" zurück, die Anzahl der Zeichen
}
```

## Weiterführende Informationen:

1. [Go-Dokumentation für die eingebaute Funktionen](https://golang.org/pkg/builtin/): Bietet tiefergehende Informationen zu den eingebauten Funktionen von Go, einschließlich `len()`.
2. [Go Strings Artikel](https://blog.golang.org/strings): Erläutert ausführlich, wie Strings in Go implementiert werden, einschließlich der Behandlung von Unicode-Zeichen.
3. [Golang UTF-8 Paket](https://golang.org/pkg/unicode/utf8/): Details zur verwendeten `RuneCountInString()` Funktion, um echte Zeichen in UTF-8 codierten Strings zu zählen.