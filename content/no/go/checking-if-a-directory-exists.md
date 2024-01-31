---
title:                "Sjekke om en mappe eksisterer"
date:                  2024-01-20T14:56:34.810625-07:00
simple_title:         "Sjekke om en mappe eksisterer"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Å sjekke om en mappe finnes er det å verifisere at den spesifiserte stien peker til noe som eksisterer på filsystemet. Programmerere gjør dette for å unngå feil som oppstår når kode prøver å aksessere eller skrive til en ikke-eksisterende mappe.

## How to: (Slik gjør du:)
```go
package main

import (
    "fmt"
    "os"
)

func main() {
    path := "./eksempelmappe"

    if _, err := os.Stat(path); os.IsNotExist(err) {
        fmt.Printf("Mappen %s finnes ikke.\n", path)
    } else {
        fmt.Printf("Mappen %s finnes.\n", path)
    }
}
```

Kjører du denne koden vil utskriften enten være `"Mappen ./eksempelmappe finnes ikke."` eller `"Mappen ./eksempelmappe finnes."` avhengig av om mappen eksisterer.

## Deep Dive (Dypdykk)
Historisk har forskjellige programmeringsspråk tilbudt diverse måter å sjekke om mapper eksisterer. Go har en innebygd `os` pakke som gir tilgang til filsystemfunksjoner, inkludert å sjekke om filer og mapper eksisterer. Alternativer til `os.Stat` inkluderer bruk av `os.IsExist` eller tredjeparts biblioteker som tilbyr mer abstrakte filhåndteringsfunksjoner. Detaljene i `os.Stat` kaller `stat` eller `lstat` systemkallene bak kulissene, avhengig av operativsystemet, for å skaffe filinformasjon, noe som er vanlig praksis for filsysteminteraksjoner på lavt nivå.

## See Also (Se også)
- [os package documentation](https://pkg.go.dev/os)
- [Working with filesystems in Go](https://golang.org/doc/)
- [Filepath package for working with file paths](https://pkg.go.dev/path/filepath)
