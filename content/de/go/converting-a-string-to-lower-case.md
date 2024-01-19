---
title:                "Einen String in Kleinbuchstaben umwandeln"
html_title:           "Elm: Einen String in Kleinbuchstaben umwandeln"
simple_title:         "Einen String in Kleinbuchstaben umwandeln"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Strings in Kleinbuchstaben Umwandeln mit Go

## Was & Warum?

Das Umwandeln eines Strings in Kleinbuchstaben hilft dabei, Code und Benutzereingaben zu standardisieren und Vergleiche zu ermöglichen. Programmierer machen das, um Groß- und Kleinschreibungsunabhängigkeit zu erreichen.

## Wie geht das?

Mit Go können wir die `ToLower` Funktion aus dem `strings` Paket verwenden. Es macht es einfach, einen String in Kleinbuchstaben umzuwandeln.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "Hallo Welt!"
	lowercase := strings.ToLower(str)
	fmt.Println(lowercase)
}
```
Beim Ausführen gibt dieser Code "hallo welt!" auf der Konsole aus.

## Vertiefung

Die `ToLower` Funktion in Go wurde erstmals in der ersten öffentlichen Version Go 1 eingeführt. Es gibt wenige Alternativen zum `ToLower` Funktion, wie das manuelle Durchlaufen jedes Zeichens im String und Anwenden der `unicode.ToLower` Funktion darauf, aber das ist ineffizient und unpraktisch.

Die `ToLower` Funktion implementiert einen effizienten Algorithmus, der direkt auf einer Kopie der Ursprungszeichenkette arbeitet und sie so in Kleinbuchstaben umwandeln kann. 

```Go
func ToLower(s string) string {
	return Map(unicode.ToLower, s)
}
```

Die Funktion nutzt eine Map-Funktion, um `unicode.ToLower` auf jeden Buchstaben im String anzuwenden. 

## Siehe Auch

Für eine detailliertere Erklärung zur `ToLower` Funktion und verwandten Funktionen, besuchen Sie die offizielle Go Dokumentseite: [https://golang.org/pkg/strings/#ToLower](https://golang.org/pkg/strings/#ToLower).