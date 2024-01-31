---
title:                "Umformung eines Strings in Kleinbuchstaben"
date:                  2024-01-20T17:38:29.910075-07:00
model:                 gpt-4-1106-preview
simple_title:         "Umformung eines Strings in Kleinbuchstaben"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?
Beim Umwandeln eines Strings in Kleinbuchstaben ersetzt man jeden Großbuchstaben durch den entsprechlichen Kleinbuchstaben. Das ist nützlich für Vergleiche oder Verarbeitung von Text, wo Schreibweise irrelevant sein soll.

## How to:
```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	original := "Das ist EIN Test!"
	lowercase := strings.ToLower(original)
	fmt.Println(lowercase) // Ausgabe: das ist ein test!
}
```

## Deep Dive
In den frühen Tagen der Informatik wurden Textdaten oft mit uneinheitlicher Schreibweise gespeichert, was Vergleiche komplex machte. Die `strings.ToLower` Funktion in Go standardisiert Schreibweisen, sodass effizienter verglichen und gesucht werden kann. Man könnte auch jeden Buchstaben durchrangehen und umwandeln, doch das ist weniger effektiv. Es ist zu beachten, dass es für einige Sprachen, wie zum Beispiel Türkisch, mehr Regeln gibt, um spezielle Fälle abzudecken, die Go's `ToLower` möglicherweise nicht berücksichtigt.

## Siehe Auch
- Go Docs für strings Paket: https://pkg.go.dev/strings
- Unicode Standard: http://www.unicode.org/standard/standard.html
- Go Blog zum Thema Strings: https://blog.golang.org/strings
