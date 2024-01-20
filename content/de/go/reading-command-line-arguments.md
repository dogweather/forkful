---
title:                "Befehlszeilenargumente lesen"
html_title:           "Arduino: Befehlszeilenargumente lesen"
simple_title:         "Befehlszeilenargumente lesen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Lesen von Befehlszeilenargumenten ist ein wesentlicher Teil der Programmiererfahrung. Es erlaubt Ihnen, Ihrem Programm beim Start Parameter zu übergeben. Dies ermöglicht mehr Flexibilität und eine interaktive Benutzerfahrung.

## So geht's:

In Go verwenden wir das Paket "os" zum Lesen von Befehlszeilenargumenten. Hier ist ein einfaches Beispiel:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	arg := os.Args[1:]      // Argumente bekommen
	fmt.Println(arg)        // Ausgabe der Argumente
}
```

Führen Sie das Programm mit zusätzlichen Argumenten aus:

```bash
> go run main.go Hallo Welt
```

Ausgabe:

```bash
[Hallo Welt]
```

## Tiefgang

Historisch gesehen wurden Befehlszeilenargumente in C eingeführt und sind seitdem in den meisten Programmiersprachen vorhanden. Go, welches stark von C und Unix beeinflusst ist, verfügt natürlich über ähnliche Funktionalität.

Es gibt auch Alternativen zum Lesen von Argumenten mit der Bibliothek "flag", die eine umfassendere Funktionalität bietet, einschließlich der Vereinfachung der Parsierung von Argumenten in bestimmten Typen.

Im Detail hat os.Args eine Liste von Strings, mit der das Programm ausgeführt wurde. Der erste Eintrag, os.Args[0], ist der Name, wie es aufgerufen wurde. Die nachfolgenden Einträge sind die Argumente, die tatsächlich auf der Befehlszeile übergeben wurden.

## Siehe auch

Mehr Info zu `os` Paket:
- https://golang.org/pkg/os/#pkg-index

Mehr Info zur Bibliothek 'flag':
- https://golang.org/pkg/flag/

Grundlagen der Befehlszeile:
- https://de.wikipedia.org/wiki/Befehlszeile