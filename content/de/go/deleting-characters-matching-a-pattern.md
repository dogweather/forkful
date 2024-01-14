---
title:    "Go: Löschen von Zeichen, die einem Muster entsprechen"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Warum?

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann sinnvoll sein, um ungewollte Daten zu entfernen oder um eine bestimmte Formatierung zu erzielen.

## Wie?

Um Zeichen in Go anhand eines Musters zu löschen, können wir die `strings` Bibliothek verwenden. Dazu müssen wir zunächst den String in ein Array von Runen konvertieren und dann durch das Array iterieren, um die Zeichen zu überprüfen. Wenn ein Zeichen dem Muster entspricht, können wir es aus dem Array löschen und den übrigen Teil wieder in einen String zurückkonvertieren.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	s := "Hallo, Gophers!"
	pattern := "o"

	// String in Array von Runen konvertieren
	runes := []rune(s)
	// Neue leere Variable für das neue Array erstellen
	var newRunes []rune
	
	for _, r := range runes {
		// Überprüfen, ob das Zeichen dem Muster entspricht
		if string(r) == pattern {
			// Überspringen, wenn das Zeichen dem Muster entspricht
			continue
		}
		// Dem neuen Array das aktuelle Zeichen hinzufügen
		newRunes = append(newRunes, r)
	}
	// Runen-Array wieder in einen String konvertieren
	newString := string(newRunes)
	
	fmt.Println(newString) // Halle, Gphers!
}
```

## Deep Dive

Dieser Ansatz funktioniert für einfache Muster, es kann jedoch komplexer werden, wenn das Muster mehrere Zeichen enthält oder auch Regex verwendet werden soll. In solchen Fällen ist es möglicherweise sinnvoll, die `regexp` Bibliothek zu verwenden. Diese bietet vielfältige Funktionen zum Finden und Ersetzen von Mustern in Strings.

## Siehe auch

- [Offizielle Go Dokumentation](https://golang.org/doc/)
- [Gönn dir Go: Einfache Reguläre Ausdrücke in Go](https://medium.com/@deepakneo1122/g%C3%B6nn-dir-go-einfache-regul%C3%A4re-ausdr%C3%BCcke-in-go-f83f56d332f3)
- [Eine Einführung in die Go regexp Bibliothek](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-go-de#comparing-text)