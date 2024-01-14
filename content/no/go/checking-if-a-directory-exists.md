---
title:                "Go: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Hvorfor

Når vi utvikler programmer i Go, er det viktig å sørge for at koden vår er pålitelig og feilfri. En vanlig feil som kan oppstå er å prøve å aksessere en mappe som ikke eksisterer. Derfor er det viktig å kunne sjekke om en mappe faktisk eksisterer før man prøver å aksessere den.

# Hvordan

For å sjekke om en mappe eksisterer, bruker vi funksjonen `os.Stat()` og sjekker om den returnerer en feil eller ikke. Hvis det ikke er noen feil, betyr det at mappen eksisterer.

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Sjekker om mappen "test" eksisterer
    _, err := os.Stat("test")

    if err == nil {
        fmt.Println("Mappen eksisterer")
    } else {
        fmt.Println("Mappen eksisterer ikke")
    }
}
```

Output:
```
Mappen eksisterer ikke 
```

# Dypdykk

Vi kan også bruke `os.IsExist()` og `os.IsNotExist()` funksjonene for å sjekke nærmere på resultatet av `os.Stat()`. Disse funksjonene returnerer henholdsvis `true` og `false` basert på mappens eksistensstatus.

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Sjekker om mappen "test" eksisterer
    info, err := os.Stat("test")

    if os.IsExist(err) {
        fmt.Printf("Filinformasjon: %+v", info)
    } else if os.IsNotExist(err) {
        fmt.Println("Mappen eksisterer ikke")
    }
}
```

Output:
```
Mappen eksisterer ikke
```

# Se også

- [Offisiell dokumentasjon for `os.Stat()`](https://golang.org/pkg/os/#Stat)
- [Golang tutorial: Sjekke om en fil eller mappe eksisterer](https://www.golangprograms.com/golang-program-to-check-if-a-file-exists-in-a-location.html)