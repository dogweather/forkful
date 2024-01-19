---
title:                "Lese en tekstfil"
html_title:           "C#: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Lesing av tekstfiler i Go: En Praktisk Guide

## Hva & Hvorfor?
Å lese en tekstfil er prosessen med å hente data lagret i en fil i tekstformat. Programmerere gjør dette for å håndtere og manipulere denne dataen, enten for å levere informasjon til brukerne eller tillate videre databehandling.

## Hvordan:
Go har innebygde pakker som `ioutil` og `os` som gjør filhåndtering veldig enkel.

```Go
package main

import (
	"fmt"
	"io/ioutil"
)

func main() {
	data, err := ioutil.ReadFile("test.txt")
	if err != nil {
		fmt.Println("Fil lesefeil", err)
		return
	}
	fmt.Println("Innhold i fil:", string(data))
}
```
Eksempelutgang: `Innhold i fil: Hei, verden!`

Her er et annet eksempel ved å bruke `os` pakken:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	file, err := os.Open("test.txt")
	if err != nil {
		fmt.Println("Fil åpnefeil", err)
		return
	}
	defer file.Close()

	buf := make([]byte, 1024)
	for {
		n, err := file.Read(buf)
		if err != nil {
			fmt.Println(err)
			return
		}
		if n == 0 {
			break
		}
		fmt.Println(string(buf[:n]))
	}
}
```

## Dybdeplunge
- Historisk: Go ble designet på Google for å løse konkrete problemer med store systemer. Enkel fillesing var en viktig funksjon som ble inkludert fra begynnelsen.

- Alternativ: Det er flere tredjepartspakker, som `bufio` og `scanner`, som også kan brukes til lesen filer i Go. Noen kan tilby mer funksjonalitet, men standardpakken er mer enn tilstrekkelig for de fleste bruksområder.

- Implementering: Detaljer om hvordan Go håndterer fil IO ligger i kilden til `os` og `ioutil` pakkene. Disse pakkene interagerer med operativsystemet gjennom systemkall for å åpne, lese, og lukke filer.

## Se Også
- Offisielle Go-dokumenter om pakken `os`: https://golang.org/pkg/os/
- Offisielle Go-dokumenter om pakken `io/ioutil`: https://golang.org/pkg/io/ioutil/
- Detaljert tutorial om filhåndtering i Go: https://golangbot.com/read-files/