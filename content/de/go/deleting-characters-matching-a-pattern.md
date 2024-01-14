---
title:    "Go: Musterübereinstimmenden Zeichen löschen"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann eine einfache und effektive Möglichkeit sein, Daten zu bereinigen oder bestimmte Informationen aus einer Datei zu entfernen.

## Wie geht das?

Um Zeichen gemäß einem Muster zu löschen, können wir die `strings.ReplaceAll()` Funktion in Go verwenden. Zum Beispiel können wir mit dem folgenden Code alle Vokale aus einem String entfernen:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	text := "Hallo, wie geht es dir?"
	modifiedText := strings.ReplaceAll(text, "a", "")
	modifiedText = strings.ReplaceAll(modifiedText, "e", "")
	modifiedText = strings.ReplaceAll(modifiedText, "i", "")
	modifiedText = strings.ReplaceAll(modifiedText, "o", "")
	modifiedText = strings.ReplaceAll(modifiedText, "u", "")

	fmt.Println(modifiedText)
}

// Output: Hll, wh gt s dr?
```

Wir können auch reguläre Ausdrücke verwenden, um ein bestimmtes Muster zu definieren. Im folgenden Beispiel entfernen wir alle Zahlen aus einem String:

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	text := "Dies ist ein Text mit 123 Zahlen."
	regex := regexp.MustCompile("[0-9]+")
	modifiedText := regex.ReplaceAllString(text, "")

	fmt.Println(modifiedText)
}

// Output: Dies ist ein Text mit Zahlen.
```

## Tiefer Einblick

Das Löschen von Zeichen gemäß einem Muster kann sehr nützlich sein, wenn es darum geht, Daten zu bereinigen oder zu formatieren. Mit regulären Ausdrücken können wir sehr spezifische Muster definieren, um nur bestimmte Zeichen zu entfernen. Man könnte auch verschiedene Funktionen und Methoden von Go nutzen, um die Funktionalität noch weiter anzupassen. Das Löschen von Zeichen gemäß einem Muster ist eine effektive Methode, um Daten schnell und einfach zu manipulieren.

## Siehe auch

- [Official Go documentation on strings.ReplaceAll()](https://golang.org/pkg/strings/#ReplaceAll)
- [Regular expressions in Go](https://golang.org/pkg/regexp/)
- [String manipulation in Go](https://go.dev/blog/strings)