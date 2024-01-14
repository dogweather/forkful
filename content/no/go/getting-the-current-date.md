---
title:    "Go: Å få dagens dato"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Hvorfor

Det å få tak i den nåværende datoen kan være nyttig i mange programmeringsscenarier. For eksempel kan det være viktig å vise riktig dato i et program som håndterer datoavhengige operasjoner som forfallsdatoer eller aldersverifisering. Å få tak i den aktuelle datoen kan også være nyttig for å logge og spore aktiviteter i et program. Uansett hva årsaken er, er det viktig å vite hvordan man henter den nåværende datoen i Go-programmeringsspråket.

## Slik gjør du det

I Go er det enkelt å få tak i den nåværende datoen ved å bruke time-pakken og dens funksjon `Now()`. Se på følgende kodeeksempel:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    now := time.Now()
    fmt.Println("Dagens dato er:", now.Format("January 2, 2006"))
}
```

I dette eksempelet importerer vi både `fmt` og `time` pakker. Deretter bruker vi funksjonen `Now()` fra `time` pakken for å få tak i den nåværende datoen. Vi lagrer datoen i en variabel `now` og bruker deretter `Format()` funksjonen for å formatere datoen i ønsket format. I dette tilfellet har vi brukt "January 2, 2006" som formatet for å få datoen i form av "Måned Dato, År".

Når du kjører dette programmet, vil du få følgende utgang:

```sh
Dagens dato er: September 4, 2021
```

Hvis du ønsker å få datoen i et annet format, kan du prøve forskjellige kombinasjoner av tidsformatet fra dokumentasjonen til Go-pakken.

## Dykke dypere

I tillegg til `Now()` funksjonen, har Go også andre nyttige funksjoner for å håndtere dato og tid, som for eksempel `Date()` og `Time()`. Disse funksjonene tillater deg å hente ut spesifikke verdier som år, måned, dag, timer, minutter og sekunder fra en tidspunktvariabel.

For å lære mer om disse funksjonene og hvordan du kan bruke dem, kan du se på dokumentasjonen for `time` pakken på Go sin offisielle nettside.

## Se også

- [Go sin offisielle nettside](https://golang.org/)
- [Dokumentasjon for time-pakken i Go](https://pkg.go.dev/time)