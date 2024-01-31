---
title:                "Organisering av kode i funksjoner"
date:                  2024-01-26T01:10:16.241004-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organisering av kode i funksjoner"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?
Organisering av kode i funksjoner handler om å bryte ned koden din i gjenbrukbare deler. Det gjør koden din ryddigere, lettere å lese og enklere å feilsøke.

## Hvordan:
Her er et Go-utdrag som viser en kodeblokk, etterfulgt av en refaktorert versjon som bruker funksjoner:

```go
package main

import "fmt"

func main() {
    // Før: Innbakt kode
    fmt.Println("Beregner sum...")
    total := 0
    for i := 1; i <= 10; i++ {
        total += i
    }
    fmt.Println("Total sum er:", total)

    // Etter: Bruk av en funksjon
    fmt.Println("Beregner sum ved hjelp av en funksjon...")
    sum := getSum(1, 10)
    fmt.Println("Total sum er:", sum)
}

// Funksjon for å beregne sum innenfor et område
func getSum(start, slutt int) int {
    total := 0
    for i := start; i <= slutt; i++ {
        total += i
    }
    return total
}
```

Eksempelutskriften for både innbakt kode og funksjonsbasert kode vil være den samme:

```
Beregner sum...
Total sum er: 55
Beregner sum ved hjelp av en funksjon...
Total sum er: 55
```

## Dypdykk
Før konseptet med funksjoner kom, var programmering i stor grad prosedyremessig, med kode som kjørte fra topp til bunn. Ettersom programmene vokste, frembrakte denne tilnærmingen ineffektivitet og kodegjentakelse.

Språk introduserte funksjoner som en mekanisme for abstraksjon. I Go innkapsler funksjoner kodeblokker med en spesifikk oppgave, noe som oppmuntrer til DRY-prinsippet (Don't Repeat Yourself). De aksepterer parametere og kan returnere resultater.

Nyttige tips:
- Gi funksjoner klare navn; et godt navn forklarer hva en funksjon gjør.
- Hold dem korte; hvis en funksjon gjør for mye, bryt den ned.
- Funksjoner kan returnere flere verdier, utnytt dette for feilhåndtering.
- Funksjoner av høyere orden (funksjoner som tar imot eller returnerer andre funksjoner) er kraftige verktøy i Go.

Alternativer til funksjoner inkluderer innbakt kode (rotete for komplekse oppgaver) og objektmetoder (en del av det objektorienterte paradigmet som er tilgjengelig i Go gjennom strukturer).

## Se Også
- [Go by Example: Functions](https://gobyexample.com/functions)
- [Effective Go: Function](https://golang.org/doc/effective_go#functions)
