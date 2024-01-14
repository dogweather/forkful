---
title:    "Go: Sjekker om en mappe eksisterer"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor
Å sjekke om en mappe eksisterer kan være nyttig når du utvikler programvare for å sikre at filer som du forventer å finne er på riktig sted. Det kan også hjelpe deg med å unngå feil når du prøver å åpne eller lese filer.

## Hvordan
```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Sjekk om mappen med navnet "testmappe" eksisterer
    _, err := os.Stat("testmappe")

    // Hvis det ikke oppstår noen feil, betyr det at mappen eksisterer
    if err == nil {
        fmt.Println("Mappen eksisterer")
    } else {
        fmt.Println("Mappen eksisterer ikke")
    }
}
```
### Eksempel utgang:
```
Mappen eksisterer
```

## Dypdykk
Når du sjekker om en mappe eksisterer ved hjelp av `os.Stat` funksjonen, returnerer den en `os.FileInfo` struct. Denne structen inneholder informasjon som navnet på filen/mappen, størrelse, endret dato og mye mer. Du kan bruke denne informasjonen til å utføre andre handlinger, som for eksempel å få tak i alle filene som ligger inne i mappen.

## Se også
- [Go Dokumentasjon for os.Stat](https://golang.org/pkg/os/#Stat)
- [Go Eksempler for å sjekke om en mappe eksisterer](https://gobyexample.com/file-exists)