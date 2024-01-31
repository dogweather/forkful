---
title:                "Lese kommandolinjeargumenter"
date:                  2024-01-20T17:56:03.067401-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lese kommandolinjeargumenter"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å lese kommandolinjeargumenter i Go betyr å hente inn input direkte fra kommandolinjen når du kjører et program. Vi gjør dette for å lage mer fleksible og interaktive programmer hvor brukeren kan påvirke kjøringen uten å endre koden.

## How to:
Her er det grunnleggende. Dette Go-scriptet leser argumentene du sender inn.

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	args := os.Args[1:] // Hopp over programnavnet
	for _, arg := range args {
		fmt.Println(arg)
	}
}
```

Kjør med: `go run . arg1 arg2`, og se dette som utskrift: 

```
arg1
arg2
```

## Deep Dive
Kommandolinjeargumenter har vært i spill siden de gamle dagene av Unix. Go holder det enkelt med `os`-pakken, som har vært del av standardbiblioteket siden starten. Alternativer? Du har `flag`, for mer komplekse behov. Under panseret jobber `os.Args` som en slice, så operasjoner som slicing og iterating er lette å gjøre.

## See Also
- Go by Example: https://gobyexample.com/command-line-arguments
- Go Docs for `flag` package: https://pkg.go.dev/flag 
- Go Blog on `os` package: https://blog.golang.org/using-go-modules
