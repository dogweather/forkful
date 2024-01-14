---
title:                "Go: Eine Zeichenkette großschreiben"
simple_title:         "Eine Zeichenkette großschreiben"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum
In der Programmierung gibt es oft die Notwendigkeit, Strings großzuschreiben. Dies kann zum Beispiel bei der Benutzereingabe oder beim Formatieren von Texten notwendig sein. In diesem Blog-Beitrag werden wir uns ansehen, wie man Strings in Go großschreibt und warum es wichtig sein kann.

## Wie geht das
In Go gibt es eine eingebaute Funktion namens "Capital" , die uns dabei hilft, Strings zu großzuschreiben. Sie akzeptiert einen String als Parameter und gibt den großgeschriebenen String zurück. Hier ist ein Beispielcode, der dies zeigt:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	message := "hallo welt!"
	fmt.Println(strings.Title(message))
}
```

Die Ausgabe dieses Codes wird "Hallo Welt!" sein. Wie Sie sehen können, wird die Funktion Title() von der Standardbibliothek "strings" aufgerufen.

## Tiefentauchen
Man kann auch einen eigenen Algorithmus schreiben, um Strings in Go großzuschreiben. Hier ist ein Beispiel, das Buchstaben für Buchstaben durchläuft und sie in Großbuchstaben umwandelt:

```Go
func capitalize(str string) string {
	var upperCaseStr string
	for _, ch := range str {
		upperCaseCh := strings.ToUpper(string(ch))
		upperCaseStr += upperCaseCh
	}
	return upperCaseStr
}

func main() {
	message := "hallo welt!"
	fmt.Println(capitalize(message))
}
```
Die Ausgabe dieses Codes wird wiederum "HALLO WELT!" sein. Beachten Sie, dass diese Methode nicht alle Sprachzeichen korrekt großschreibt und daher nicht universell anwendbar ist.

## Siehe auch
- [Go String Dokumentation](https://golang.org/pkg/strings/#Title)
- [Converting string to uppercase in Go](https://golangdocs.com/convert-string-to-uppercase-in-golang)
- [8 Neat Tricks with Go Strings](https://medium.com/factory-mind/regex-tutorial-a-simple-cheatsheet-by-examples-649dc1c3f285)