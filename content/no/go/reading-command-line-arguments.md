---
title:                "Go: Lese argumenter fra kommandolinjen"
simple_title:         "Lese argumenter fra kommandolinjen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen ønske å lese kommandolinjeargumenter i et Go-program? Det kan være flere grunner til dette. Kanskje du vil lage et program som kan ta ulike typer input fra brukeren, eller kanskje du vil lage et program som kan håndtere ulike kommandoer fra en terminal. Uansett årsak, å lære å lese kommandolinjeargumenter vil være nyttig for enhver Go-utvikler.

## Slik gjør du det

For å lese kommandolinjeargumenter i et Go-program, må du først importere `os`- og `flag`-pakken. Deretter kan du bruke `os.Args` for å få en liste over alle argumentene som er gitt til programmet. Du kan også bruke `flag`-pakken for å definere spesifikke flagg og behandle dem i koden din.

La oss se på et eksempel:

```Go
package main

import (
	"flag"
	"fmt"
	"os"
)

func main() {
	// Definerer flagg ved hjelp av flag-pakken
	languages := flag.String("languages", "Go", "a comma-separated list of programming languages")

	// Parsing av flagg
	flag.Parse()

	// Henter argumenter fra os.Args
	args := os.Args[1:]

	// Printer ut argumentene gitt til programmet
	fmt.Println("Argumenter:", args)

	// Printer ut verdiene til flaggene
	fmt.Println("Språk:", *languages)
}
```

Hvis du kjører dette programmet fra terminalen med argumenter og flagg, vil du få følgende utskrift:

```bash
$ go run main.go hello world -languages=Java,Python
Argumenter: [hello world]
Språk: Java,Python
```

Som du kan se, vil argumentene vi gir til programmet være tilgjengelige i `os.Args`. I tillegg kan vi bruke flaggene vi har definert i programmet vårt ved å bruke `flag`-pakken.

## Dykk dypere

For å lære mer om å lese kommandolinjeargumenter i Go, kan du se nærmere på `os.Args` og `flag`-pakken. `os.Args` gir også tilgang til andre nyttige funksjoner for å håndtere kommandolinjeinput, som for eksempel `os.Args[0]` som gir navnet på det kjørbare filen.

I tillegg kan du også utforske mer avanserte konsepter som å bruke subkommandoer med `flag`-pakken for å håndtere komplekse kommandoer og argumenter.

## Se også

- [Go's Official Documentation on `os` Package](https://golang.org/pkg/os/)
- [Go's Official Documentation on `flag` Package](https://golang.org/pkg/flag/)
- [Command Line Input in Go Tutorial](https://gobyexample.com/command-line-arguments)