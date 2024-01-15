---
title:                "Sjekke om en mappe eksisterer"
html_title:           "Go: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sjekke om en mappe eksisterer er en viktig del av programmering. Dette lar deg sikre at koden din fungerer som forventet og at du unngår feil når du prøver å få tilgang til ikke-eksisterende mapper.

## Hvordan

Sjekke om en mappe eksisterer i Go er en enkel prosess. Du kan bruke funksjonen *os.Stat()* som returnerer informasjon om en gitt fil eller mappe. Hvis mappen ikke eksisterer, vil denne funksjonen returnere en feil.

```Go
package main

import (
	"os"
	"fmt"
)

func main() {
	fmt.Println("Sjekker eksistensen av en mappe...")

	// Bytt ut "navn_på_mappen" med navnet på mappen du vil sjekke
	if _, err := os.Stat("navn_på_mappen"); err != nil {
		if os.IsNotExist(err) {
			fmt.Println("Mappen finnes ikke.")
		} else {
			fmt.Println("En annen feil oppstod.")
		}
	} else {
		fmt.Println("Mappen eksisterer.")
	}
}
```

For eksempel, hvis vi kjører dette programmet og mapper eksisterer, vil output bli: "Mappen eksisterer." Hvis mappen ikke eksisterer, vil output bli: "Mappen finnes ikke." Hvis en annen feil oppstår, vil output bli: "En annen feil oppstod."

## Dypdykk

I tillegg til *os.Stat()* funksjonen, kan du også bruke *os.IsNotExist()* og *os.IsExist()* for å sjekke spesifikke typer feil. Disse funksjonene tar inn en feil som parameter og returnerer en bool-verdi som indikerer om feilen er relatert til mappen som ikke eksisterer eller allerede eksisterer.

Det er også verdt å merke seg at *os.Stat()* vil også returnere informasjon om alle typer filer, ikke bare mapper. Derfor kan det være lurt å inkludere en ekstra sjekk for å sikre at filen du sjekker er en mappe. Dette kan gjøres ved å bruke *os.IsDir()* som vil returnere en bool-verdi basert på om den gitt banen er en mappe eller ikke.

## Se også

- [Dokumentasjon for os-pakken i Go](https://golang.org/pkg/os/)
- [Go Tutorial - Working with files and directories](https://www.callicoder.com/golang-work-with-files/)
- [Managing Files and Directories in Go](https://blog.learngoprogramming.com/how-to-manage-files-and-directories-in-go-bfe7dd89a4f7)