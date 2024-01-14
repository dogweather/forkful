---
title:    "Go: Verwendung von regulären Ausdrücken"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Reguläre Ausdrücke sind ein leistungsstarkes Werkzeug in der Go-Programmierung. Sie ermöglichen es uns, komplexe Muster in Zeichenketten zu suchen und zu manipulieren. Wenn Sie sich mit der Verarbeitung von Texten in Ihrer Anwendung beschäftigen, können reguläre Ausdrücke Ihnen viel Zeit und Aufwand sparen.

## Wie

Um reguläre Ausdrücke in Go zu verwenden, müssen Sie zunächst das "regexp" Package importieren. Dann können Sie die Funktionen des Packages verwenden, um Ausdrücke zu erstellen, zu suchen und zu manipulieren.

Eine einfache Anwendung von regulären Ausdrücken ist die Suche nach einer bestimmten Zeichenfolge in einer Zeichenkette. Zum Beispiel können wir nach allen Vorkommen von "Hallo Welt" in einem Text suchen:

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	str := "Herzlich willkommen in der wundervollen Welt der Go-Programmierung!"
	re := regexp.MustCompile("Welt")
	fmt.Println(re.FindString(str))
}
```

Dies gibt uns folgende Ausgabe:

```
Welt
```

Wir können aber auch komplexere Ausdrücke erstellen, um bestimmte Muster zu suchen und zu manipulieren. Beispielsweise können wir alle Wörter in einem Text in Großbuchstaben ändern:

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	str := "Hallo Welt, dies ist ein Beispieltext."
	re := regexp.MustCompile(`\b\w`)
	fmt.Println(re.ReplaceAllStringFunc(str, strings.ToUpper))
}
```

Dies gibt uns folgende Ausgabe:

```
HALLO WELT, DIES IST EIN BEISPIELTEXT.
```

## Tiefentauchgang

Es gibt viele verschiedene Möglichkeiten, reguläre Ausdrücke in Go zu nutzen. Sie können nicht nur Muster suchen und manipulieren, sondern auch Validierungen durchführen, Teile von Zeichenketten extrahieren und vieles mehr.

Hier sind einige weitere Ressourcen, die Ihnen helfen können, tiefer in die Welt der regulären Ausdrücke in Go einzutauchen:

- [Die offizielle Dokumentation von Go](https://golang.org/pkg/regexp/)
- [Ein interaktiver Kurs zu regulären Ausdrücken in Go](https://regexone.com/references/go)
- [Ein ausführliches Tutorial von DigitalOcean](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-go-de)

## Siehe auch

- [Veranschaulichung der Go Documentation](https://blog.golang.org/effective-go)
- [Übersicht des Markdown-Formats](https://www.markdownguide.org/basic-syntax)