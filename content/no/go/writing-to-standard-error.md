---
title:                "Skrive til standardfeil"
html_title:           "Arduino: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skrive til standard feil (`stderr`) flytter feilmeldinger og diagnostikk bort fra vanlig utdata (`stdout`). Vi gjør det for å skille normalt resultat fra feil, slik at det blir lettere å oppdage og håndtere feil.

## Hvordan gjøre det:
```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	_, err := os.Open("ikke-eksisterende-fil.txt")
	if err != nil {
		fmt.Fprintln(os.Stderr, "Feil ved åpning av fil:", err)
	}
}
```

Eksempel utdata:
```
Feil ved åpning av fil: open ikke-eksisterende-fil.txt: no such file or directory
```

## Dypdykk
Historisk sett, skiller man mellom `stdout` og `stderr` for å tillate omdirigering i kommandolinje-interfaces. Alternativer for skriving til `stderr` inkluderer logger-biblioteker, som kan gi mer funksjonalitet. `os.Stderr` er en global variabel i Go’s os-pakke, implementert som en `*os.File`, og kan brukes akkurat som vanlige filer til I/O-operasjoner.

## Se også
- Go dokumentasjon for `os` pakken: https://golang.org/pkg/os/
- Go blogg om feilhåndtering: https://blog.golang.org/error-handling-and-go
- Unix filsystem og strømninger: http://www.tldp.org/LDP/Linux-Filesystem-Hierarchy/html/stdout.html