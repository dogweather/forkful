---
title:                "Sjekker om en mappe eksisterer"
html_title:           "Go: Sjekker om en mappe eksisterer"
simple_title:         "Sjekker om en mappe eksisterer"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sjekke om en mappe eksisterer i programmering er å bekrefte om en gitt filsti peker til en gyldig katalog. Dette er viktig for å unngå feil som kan oppstå når du prøver å manipulere mappen (for eksempel lese eller skrive til den) hvis den ikke finnes.

## Hvordan du gjør det:
Her er en kortfattet Go kodebit som sjekker om en mappe eksisterer:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    _, err := os.Stat("/path/to/directory")

    if os.IsNotExist(err) {
        fmt.Println("Mappen eksisterer ikke")
    } else {
        fmt.Println("Mappen eksisterer")
    }
}
```
Utfører du denne koden, vil output være enten `Mappen eksisterer ikke` eller `Mappen eksisterer`, avhengig av om mappen eksisterer.

## Dyp Dykking
Go-språket har en innebygget pakke kalt `os` for å håndtere operativsystemrelaterte funksjoner, som fil- og mappeoperasjoner. Metoden `os.Stat(path)` henter filinformasjonen. Hvis det oppstår en feil og den er av typen `os.IsNotExist`, indikerer det at mappen ikke eksisterer.
En viktig detalj å merke seg er at denne metoden vil returnere `false` både når en fil eller mappe ikke finnes, og når det oppstår en feil av hvilken som helst annen grunn.
Alternativer til denne metoden kan være å bruke tredjepartsbibliotek, men de bruker vanligvis samme metode bak kulissene og gir mer alvorlige feilhåndteringsfunksjoner.

## Se Også
For mer detaljerte informasjon og relaterte ressurser, besøk følgende linker:
1. Go offisiell dokumentasjon: [os Pakken](https://golang.org/pkg/os/)
2. Go Blogg: [Error handling and Go](https://blog.golang.org/error-handling-and-go)
3. Stack Overflow post: [How do you check if a directory exists in Go?](https://stackoverflow.com/questions/10510691/how-do-you-check-if-a-file-exists-without-opening-it-in-the-go-language)