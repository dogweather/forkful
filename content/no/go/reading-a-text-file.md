---
title:    "Go: Leser en tekstfil"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Hvorfor

Lesing av tekstfiler er en vanlig oppgave i programmering, og det er viktig å ha en god forståelse for hvordan dette gjøres. I denne bloggposten vil vi se på hvordan du kan lese en tekstfil ved hjelp av Go-programmeringsspråket, og hvorfor det er nyttig å kunne dette.

## Slik gjør du det

For å lese en tekstfil i Go, kan du bruke funksjonen "Open" fra "os" pakken. Funksjonen tar inn filnavnet som parameter og returnerer en *File* type variabel som representerer åpnet fil. Deretter kan du bruke en *Scanner* type variabel fra "bufio" pakken til å lese filen linje for linje ved hjelp av "Scan" eller "Scanln" funksjoner. Her er et eksempel på hvordan dette kan gjøres:

```Go
package main

import (
    "fmt"
    "os"
    "bufio"
)

func main() {
    file, err := os.Open("tekstfil.txt") 
    // Sjekker om det er en error
    if err != nil {
        fmt.Println(err)
        return
    }
    // Lukker filen når du er ferdig med å arbeide med den
    defer file.Close()

    // Bruker en scanner for å lese filen linje for linje
    scanner := bufio.NewScanner(file)

    // Går gjennom hver linje og printer den ut
    for scanner.Scan() {
        fmt.Println(scanner.Text())
    }

    // Sjekker for error etter at skanningen er ferdig
    if err = scanner.Err(); err != nil {
        fmt.Println(err)
        return
    }
}
```

For å kjøre dette eksempelet, må du sørge for at du har en tekstfil kalt "tekstfil.txt" i samme mappe som programmet ditt. Dersom alt går som det skal, vil programmet skrive ut innholdet i tekstfilen linje for linje.

## Dypdykk

Når du leser en tekstfil ved hjelp av Go, er det viktig å være oppmerksom på hvordan du behandler eventuelle feil. I eksempelet over brukte vi *defer* for å sørge for at filen blir lukket etter at vi er ferdige med å arbeide med den, uansett om det oppstår en feil eller ikke. Det er også viktig å huske på at *Scan* og *Scanln* funksjonene vil lese filen til slutten på hver linje, så dersom du vil ha hele innholdet i filen, må du kanskje bruke en løkke for å lese hver linje og lagre den i en variabel.

## Se også

* [Go-programmeringsspråkets offisielle nettside](https://golang.org/)
* [Dokumentasjon for "os" pakken](https://golang.org/pkg/os/)
* [Dokumentasjon for "bufio" pakken](https://golang.org/pkg/bufio/)