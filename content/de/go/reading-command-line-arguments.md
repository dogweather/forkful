---
title:                "Lesen von Kommandozeilenargumenten"
date:                  2024-01-20T17:56:06.498529-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lesen von Kommandozeilenargumenten"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?
Kommandozeilenargumente zu lesen bedeutet, Eingaben aus der Shell direkt in dein Programm zu holen. Das ist nützlich, weil es die Interaktion mit dem Benutzer erlaubt und Skripte flexibler macht.

## How to:
Hier ist ein einfaches Beispiel, wie man in Go Kommandozeilenargumente liest. Wir nutzen das `os`-Paket:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	argsWithProg := os.Args
	argsWithoutProg := os.Args[1:]

	fmt.Println("Alle Argumente inklusive Programmname:", argsWithProg)
	fmt.Println("Argumente ohne Programmname:", argsWithoutProg)
}
```
Wenn du das Programm mit `go run myprogram.go arg1 arg2` ausführst, sollte die Ausgabe so aussehen:

```
Alle Argumente inklusive Programmname: [myprogram arg1 arg2]
Argumente ohne Programmname: [arg1 arg2]
```

## Deep Dive
Kommandozeilenargumente sind so alt wie die Kommandozeile selbst. In Unix-Betriebssystemen und deren Shells werden sie schon immer genutzt. In Go greifen wir über das `os`-Paket darauf zu, welches systemunabhängig arbeitet, da es sich um ein Standardpaket handelt. Alternativen zu `os.Args` sind Flag-Parsing-Pakete wie `flag` oder Drittanbieter-Tools wie Cobra, die mehr Funktionalitäten und eine schärfere Parsing-Logik mitbringen. Mit `os.Args` erhältst du ein Slice von `string`. Dabei steht `os.Args[0]` für den Programmnamen und die weiteren Einträge für die eigentlichen Argumente. Die Indizierung beginnt bei 0, wie bei allem in Go.

## See Also
Weitere Infos und Tutorials findest du hier:
- Go by Example: Command-Line Arguments: https://gobyexample.com/command-line-arguments
- The Go Programming Language Specification: https://golang.org/ref/spec
- Go flag package: https://golang.org/pkg/flag/ 
- Cobra library: https://github.com/spf13/cobra