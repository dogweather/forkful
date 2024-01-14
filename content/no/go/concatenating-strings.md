---
title:    "Go: Sammenføyning av strenger"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kombinere eller "konkatenere" strenger er en viktig del av programmering, spesielt i språk som Go. Det lar deg dynamisk bygge tekststrenger for å gjøre koden din mer fleksibel og brukervennlig.

## Hvordan

For å konkatenere strenger i Go, kan du bruke "+" -operatøren til å legge til to strenger sammen. La oss se på et eksempel:

```Go
// Oppretter to strenger
fornavn := "Jon"
etternavn := "Doe"

// Kombinerer strengene ved hjelp av "+" -operatøren
fulltNavn := fornavn + etternavn

// Skriver ut resultatet
fmt.Println(fulltNavn)
```

Output: JonDoe

Her ser vi at vi enkelt har kombinert de to separate strengene til en ny streng ved hjelp av "+" -operatøren.

## Dypdykk

I tillegg til "+" -operatøren, kan du også bruke "fmt.Sprintf" -funksjonen til å konkatenere strenger i Go. Denne funksjonen lar deg formatere strenger basert på variabler du legger til. La oss se på et eksempel:

```Go
// Oppretter to variabler
alder := 25
tekst := fmt.Sprintf("Jeg er %d år gammel", alder)

// Skriver ut resultatet
fmt.Println(tekst)
```

Output: Jeg er 25 år gammel

Her har vi brukt variabelen "alder" til å formatere strengen "tekst" og dermed kombinere tekst og variabelen i en ny streng.

## Se også

- [Go offisiell dokumentasjon for strengekonkatering](https://golang.org/ref/spec#Operators)
- [En grundig guide til strengmanipulasjon i Go](https://blog.golang.org/strings)
- [Enkel innføring i Go fra Codecademy](https://www.codecademy.com/learn/learn-go)