---
title:                "Refaktorering"
date:                  2024-01-26T01:18:56.364763-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorering"

category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/refactoring.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Refaktorering er prosessen med å restrukturere eksisterende dataprogramkode uten å endre dens eksterne oppførsel. Programmerere gjør dette for å forbedre ikke-funksjonelle attributter av programvaren, som lesbarhet og vedlikeholdbarhet, noe som kan gjøre koden lettere å forstå, redusere kompleksiteten og hjelpe til med å oppdage feil lettere.

## Hvordan:
La oss dykke inn i et enkelt eksempel på refaktorering av Go-kode. Vi tar et utdrag som kalkulerer gjennomsnittet av et utvalg av tall og refaktorerer det for klarhet og gjenbrukbarhet.

Opprinnelig kode:
```Go
package main

import "fmt"

func main() {
    tall := []float64{8, 12, 15, 10, 7, 14}
    var sum float64
    for _, num := range tall {
        sum += num
    }
    gjennomsnitt := sum / float64(len(tall))
    fmt.Println("Gjennomsnitt:", gjennomsnitt)
}
```

Refaktorert kode:
```Go
package main

import "fmt"

// CalculateAverage tar et utvalg av float64 og returnerer gjennomsnittet.
func CalculateAverage(tall []float64) float64 {
    sum := 0.0
    for _, num := range tall {
        sum += num
    }
    return sum / float64(len(tall))
}

func main() {
    tall := []float64{8, 12, 15, 10, 7, 14}
    gjennomsnitt := CalculateAverage(tall)
    fmt.Println("Gjennomsnitt:", gjennomsnitt)
}
```

I den refaktorerte koden har vi trukket ut logikken som kalkulerer gjennomsnittet til en separat funksjon ved navn `CalculateAverage`. Dette gjør `main`-funksjonen mer kortfattet og gjennomsnittsberegningslogikken gjenbrukbar og testbar.

## Dypdykk
Refaktorering av kode er ikke et moderne konsept; det forutdaterer utbredt datamaskinbruk. Praksisen startet sannsynligvis innenfor maskinteknikk eller enda tidligere. I programvare ble det mer formalisert med fremveksten av objektorientert programmering og ekstrem programmering (XP) på 1990-tallet, særlig påvirket av Martin Fowlers banebrytende bok "Refactoring: Improving the Design of Existing Code."

Det finnes utallige refaktoreringsteknikker, fra enkel omdøping av variabler for klarhet til mer komplekse mønstre som å trekke ut metoder eller klasser. Nøkkelen er å gjøre små, inkrementelle endringer som ikke endrer programvarens funksjonalitet, men forbedrer den interne strukturen.

Når man bruker Go, kan refaktorering være enkelt på grunn av språkets enkelhet og kraftige standardbibliotek. Det er imidlertid fortsatt viktig å ha et godt sett med enhetstester for å sikre at refaktorering ikke introduserer feil. Verktøy som `gorename` og `gofmt` hjelper til med å automatisere noen av prosessene, og IDE-er har ofte innebygd støtte for refaktorering.

Foruten manuell refaktorering finnes det noen automatiserte kode refaktoreringverktøy tilgjengelige for Go, som GoLands refaktoreringverktøy og Go Refactor. Selv om disse kan fremskynde prosessen, er de ikke en erstatning for å forstå koden og gjøre gjennomtenkte endringer.

## Se også
 - [Refaktorering i Go: Enkelt er vakkert](https://go.dev/blog/slices)
 - [Effektiv Go: Refaktorering med grensesnitt](https://go.dev/doc/effective_go#interfaces)
 - [Martin Fowlers refaktoreringsside](https://refactoring.com/)
 - [GoLand Refaktoreringverktøy](https://www.jetbrains.com/go/features/refactorings/)
