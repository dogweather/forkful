---
title:                "Go: Kontrollere om en mappe eksisterer"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

### Hvorfor

Det å sjekke om en mappe eksisterer kan være en viktig del av programmeringen din. Det kan hjelpe deg med å sikre at programmet ditt kjører riktig og unngå feil som kan oppstå hvis mappen ikke finnes.

### Hvordan gjøre det

Sjekke om en mappe eksisterer i Go er enkelt og greit. Du kan bruke funksjonen `Exists` fra `os` pakken til å sjekke om en mappe eksisterer eller ikke. La oss se på et eksempel:

```Go
package main

import (
  "fmt"
  "os"
)

func main() {
  path := "minMappe"

  // Sjekker om mappen eksisterer
  if _, err := os.Stat(path); err != nil {
    if os.IsNotExist(err) {
      fmt.Println("Mappen eksisterer ikke")
    } else {
      fmt.Println("Kunne ikke sjekke mappen")
    }
  } else {
    fmt.Println("Mappen eksisterer")
  }
}
```

Her bruker vi `os.Stat` for å sjekke om en mappe eksisterer. Hvis mappen ikke eksisterer, vil vi få en `os.IsNotExist` feil og vi kan skrive en passende melding. Hvis alt går bra, vil vi få en `nil` verdi, noe som betyr at mappen eksisterer.

### Dypdykk

Hvis du vil lære mer om hvordan `os.Stat` funksjonen fungerer og hvordan den kan brukes til å sjekke andre filer eller mapper, kan du sjekke ut dokumentasjonen her: [https://golang.org/pkg/os/#Stat](https://golang.org/pkg/os/#Stat)

### Se også

- [https://golang.org/pkg/os/#IsNotExist](https://golang.org/pkg/os/#IsNotExist)
- [https://golang.org/pkg/os/#Exists](https://golang.org/pkg/os/#Exists)
- [https://gobyexample.com/directory-exists](https://gobyexample.com/directory-exists)