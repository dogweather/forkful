---
title:                "Go: Lesen von Befehlszeilenargumenten"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum
Das Lesen von Befehlszeilenargumenten ist eine wichtige Fähigkeit für jeden, der Go-Programmierung betreibt. Ob Sie eine kleine Anwendung oder ein großes Framework entwickeln, das Verständnis von Befehlszeilenargumenten ermöglicht es Ihnen, Ihre Programme flexibler und benutzerfreundlicher zu gestalten.

## Wie geht's
Um Befehlszeilenargumente in Go zu lesen, verwenden Sie einfach die `os.Args` Funktion. Diese Funktion gibt ein Array von Strings zurück, die alle vom Benutzer eingegebenen Argumente enthalten. Hier ist ein Beispielcode:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	args := os.Args
	for _, arg := range args {
		fmt.Println(arg)
	}
}

```

Angenommen, Sie haben dieses Programm als "go-arguments.go" gespeichert, können Sie es auf der Kommandozeile folgendermaßen ausführen:

```bash
go run go-arguments.go hello world
```

Die Ausgabe wird sein:

```
go-arguments ./hello
go-arguments ./world
```

Wie Sie sehen können, gibt `os.Args` auch den Namen der ausführbaren Datei (in diesem Fall "go-arguments") als ersten Eintrag in der Liste zurück. Die restlichen Einträge sind die vom Benutzer eingegebenen Argumente.

## Tiefer eintauchen
Es gibt viele Möglichkeiten, die mit Befehlszeilenargumenten in Go gemacht werden können. Zum Beispiel können Sie Flaggen verwenden, um bestimmte Funktionen oder Optionen ein- oder auszuschalten. Sie können auch Argumente in bestimmte Datentypen konvertieren und sie für andere Berechnungen verwenden.

Eine Sache zu beachten ist, dass die Reihenfolge der Argumente wichtig sein kann, je nachdem, wie Sie sie in Ihrem Code verarbeiten. Wenn Sie genau wissen wollen, warum dies der Fall ist und wie Sie damit umgehen können, empfehlen wir Ihnen, sich mit dem Konzept der "Argumentvariablen" in Go vertraut zu machen.

## Siehe auch
- Offizielle Dokumentation zu `os.Args`: https://golang.org/pkg/os/#pkg-variables
- Beispielprogramm mit Flaggen: https://gobyexample.com/command-line-flags
- Eine detaillierte Erläuterung zu Befehlszeilenargumenten in Go: https://yourbasic.org/golang/command-line-arguments/