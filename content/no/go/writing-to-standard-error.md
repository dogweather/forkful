---
title:                "Å skrive til standardfeil"
html_title:           "Go: Å skrive til standardfeil"
simple_title:         "Å skrive til standardfeil"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor
Så hvorfor skulle du egentlig bry deg om å skrive til standardfeil i Go? Kort fortalt, så er det en måte å informere om eventuelle feil eller problemer som oppstår under kjøring av ditt Go-program. Det er en viktig del av feilhåndteringen og kan hjelpe deg med å finne og fikse potensielle problemer i koden din.

## Hvordan
For å skrive til standardfeil i Go, bruker du funksjonen `fmt.Fprintf` og pekeren `os.Stderr`. Her er et eksempel på hvordan det kan gjøres:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	err := doSomething()

	// Sjekk om det oppstår en feil og skriv den til standardfeil hvis det gjør det
	if err != nil {
		fmt.Fprintf(os.Stderr, "Ops! En feil oppsto: %v", err)
	}
}
```

Kjører dette programmet vil skrive en melding til standardfeil hvis `doSomething()` returnerer en feil. 

## Dypdykk
Nå som du vet hvordan du skriver til standardfeil i Go, la oss gå litt dypere og se nærmere på sammenhengen mellom standardfeil og standardutgang. Begge disse utstrømmene er koblet til terminalen og kan leses og skrives til med `os.Stdout` og `os.Stderr` pekere. Du kan til og med kombinere disse to og skrive ut informasjon til begge samtidig ved å bruke funksjonen `fmt.Fprintf` to ganger.

## Se også
* [Go's dokumentasjon om standardfeil](https://golang.org/pkg/fmt/#Fprintf)
* [En guide til feilhåndtering i Go](https://blog.golang.org/error-handling-and-go)
* [En oversikt over grunnleggende kommandoer i Go](https://www.digitalocean.com/community/tutorials/how-to-install-go-and-set-up-a-local-programming-environment-on-macos)