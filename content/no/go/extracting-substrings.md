---
title:                "Utdrag av understrenger"
html_title:           "Bash: Utdrag av understrenger"
simple_title:         "Utdrag av understrenger"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/extracting-substrings.md"
---

{{< edit_this_page >}}

# Go programmering: Utvinne substrings

## Hva & Hvorfor?
Utvinne substrings handler om å hente ut deler av en streng. Programmerere gjør dette fordi det er nyttig å jobbe med mindre deler av en tekst, for å behandle eller analysere separate deler.

## Hvordan det gjøres:
Her er et enkelt kodeeksempel:

```Go
package main
import "fmt"

func main() {
	str := "Dette er en Golang tutoriale!"
	fmt.Println(str[10:15]) 
}
```
Output vil være:

```Go
er en
```
I eksempelet over, hentet vi ut substringen fra indeks 10 (inkludert) til indeks 15 (ekskludert) fra hovedstrengen.

## Dyp Dykk 
Substring utvinning i Go har ingen spesielle historiske implikasjoner, det har vært en standard funksjon i de fleste programmeringsspråk fra starten av.

Det finnes alternativer til utvinning av substrings i Go, men de er ofte mer komplekse og tidkrevende. Du kan for eksempel bruke regulære uttrykk eller splitting av strings etter visse tegn.

Når det gjelder implementering, bruker Go en merket struktur intern for strenger, og denne inneholder en lengde og en peker til karakterene. Ved utvinning av substrings, blit det bare laget en ny merket struktur som peker til den samme hukommelsesplassen, men med en annen lengde og startposisjon. Derfor er utvinning av substrings i Go veldig effektivt.

## Se også
For mer informasjon om hvordan jobbe med strings i Go, sjekk ut disse ressursene:
1. Go’s offisielle side om strings: https://golang.org/pkg/strings/
2. Golang Tutorial - Strings: https://golangbot.com/strings/

Ingen konklusjon seksjon, fordi du er flink og vet hva du driver med. Lykke til med programmeringen!