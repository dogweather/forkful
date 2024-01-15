---
title:                "Oppretting av en midlertidig fil"
html_title:           "Go: Oppretting av en midlertidig fil"
simple_title:         "Oppretting av en midlertidig fil"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Når man jobber med programmering, kan man ofte trenge å midlertidig lagre data i et filsystem. Dette kan være nyttig for å lagre informasjon som kun trengs midlertidig, som for eksempel cache eller mellomresultater.

## Slik gjør du det
For å opprette et midlertidig fil, kan du bruke funksjonen `os.CreateTemp()`. Denne funksjonen tar inn to argumenter, en prefiks og en suffiks, og oppretter en midlertidig fil med et tilfeldig navn som starter med prefiksen og slutter med suffiksen.

```Go
package main

import (
	"io/ioutil"
	"os"
)

func main() {
	// Oppretter en midlertidig fil med prefiksen "temp" og suffiksen ".txt"
	// Returnerer en filreferanse og en eventuell feilmelding
	fil, err := ioutil.TempFile("", "temp*.txt")
	if err != nil {
		// Hvis det oppstår en feil, skrives feilmeldingen ut og programmet stopper
		fmt.Println(err)
		os.Exit(1)
	}

	// Skriver data til den midlertidige filen
	fil.WriteString("Dette er en midlertidig fil")

	// Lukker den midlertidige filen
	fil.Close()

	// Sletter den midlertidige filen etter bruk
	os.Remove(fil.Name())
}
```

Output for dette eksempelet vil være en midlertidig fil kalt "temp469928219.txt", med teksten "Dette er en midlertidig fil".

## Dykk dypere
Når man oppretter en midlertidig fil i Go, vil den opprettes i standard temp-mappe for systemet. Denne kan vanligvis nås ved å skrive `os.TempDir()` i koden. Det er også mulig å velge en spesifikk mappe for den midlertidige filen ved å endre på den første argumenten til `CreateTemp()`-funksjonen.

## Se også
- Offisiell dokumentasjon for `os.CreateTemp()`: https://golang.org/pkg/io/ioutil/#TempFile
- Bygg en enkel webapplikasjon med Go: https://www.weheartgo.com/
- Best practices for Go programmeringsspråket: https://golang.org/doc/effective_go.html