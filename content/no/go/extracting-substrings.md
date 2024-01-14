---
title:                "Go: Uttrekking av substringer"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/extracting-substrings.md"
---

{{< edit_this_page >}}

Velkommen til vår Go programmeringsblogg! I dag skal vi se på hvordan man kan hente ut substrings i Go.

## Hvorfor
Å hente ut substrings kan være nyttig i ulike situasjoner, for eksempel når man ønsker å hente ut en del av en tekststreng eller når man skal jobbe med tekstbehandling i Go. Å kunne utføre disse operasjonene kan gjøre koden mer effektiv og lesbar.

## Hvordan
For å hente ut en substring i Go bruker vi funksjonen "Substring". Her er et eksempel på hvordan man kan hente ut en del av en tekststreng: 
```Go
str := "Hei alle sammen!"
substr := str[4:9] 
fmt.Println(substr) // Output: alle
```
Vi har her satt variabelen "str" til å være teksten "Hei alle sammen!" og definerer deretter "substr" som en substring av "str", med et startpunkt på indeks 4 og en lengde på 5 (indeks 4-8). Ved hjelp av "fmt.Println" får vi nå utskrift av substringsen "alle". 

Det er også mulig å hente ut en substring uten å angi en lengde, da vil Go automatisk ta med resten av tekststrengen fra startpunktet, som vist i følgende eksempel: 
```Go
str := "Hei alle sammen!"
substr := str[4:]
fmt.Println(substr) // Output: alle sammen!
```
Dette kan være nyttig hvis man ønsker å hente ut en del av en lengre tekststreng.

## Deep Dive
For å forstå mer teknisk hvordan henting av substrings fungerer i Go, må vi se på hvordan tekststrenger er lagret i språket. I Go er tekststrenger lagret som en sekvens av bytes som representerer hver enkelt bokstav eller tegn i strengen. Når vi bruker "Substring" funksjonen, velger vi et startpunkt og eventuelt en lengde i form av antall bytes. Dette betyr at vi kan hente ut deler av en tekststreng uavhengig av det faktiske antallet tegn i strengen.

Det er også viktig å merke seg at når man endrer substrings, så endres også den opprinnelige tekststrengen. Dette kan føre til uventede resultater hvis man ikke er klar over det.

## Se også
- [Go Dokumentasjon: Strings og substrings](https://golang.org/doc/effective_go.html#strings)
- [Introduksjon til tekstbehandling i Go](https://www.digitalocean.com/community/tutorials/how-to-use-strings-in-go)
- [Go Playground - Prøv ut eksemplene våre!](https://play.golang.org/)