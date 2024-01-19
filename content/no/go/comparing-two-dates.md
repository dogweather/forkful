---
title:                "Sammenligner to datoer"
html_title:           "Clojure: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

Title: "Sammenligne to datoer i Go"

## Hva & Hvorfor?

Å sammenligne to datoer er å avgjøre hvilken dato som kommer først ifølge kalenderen. Programmerere gjør det for å utføre tidspunktsrelaterte beregninger og beslutninger i applikasjoner.

## Hvordan:

Her er en banebrytende måte å sammenligne to datoer i Go:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	dato1 := time.Date(2022, 06, 01, 00, 0, 0, 0, time.UTC)
	dato2 := time.Date(2022, 09, 01, 00, 0, 0, 0, time.UTC)

	if dato1.Before(dato2) {
		fmt.Println("Dato1 er før dato2")
	} else if dato1.Equal(dato2) {
		fmt.Println("Dato1 er lik dato2")
	} else {
		fmt.Println("Dato1 er etter dato2")
	}
}
```

Kjør dette programmet, og utdataene vil være "Dato1 er før dato2" 

## Dyp Dykke:

1. Historisk: Før Go ble opprettet i 2007, brukte programmerere komplekse biblioteker for å jobbe med datoer. Go har forenklet ting ved å tilby innebygde pakker for dato- og tidsbehandling.
2. Alternativer: I stedet for `.Before` og `.Equals`, kan du også bruke `.After` for å sammenligne datoer.
3. Implementeringsdetaljer: `time.Date` funksjonen i Go oppretter en ny `Time` verdi, som du deretter kan manipulere med metoder som `.Before`, `.After` og `.Equal`.

## Se også:

For mer informasjon, se disse nyttige ressursene:

- Go sin offisielle dokumentasjon om tidspakken: https://golang.org/pkg/time/
- Et blogginnlegg om hvordan du jobber med datoer og tider i Go: https://www.ardanlabs.com/blog/2015/03/go-date-time-manipulation-
- En Quickstart guide til Go for nybegynnere: https://www.digitalocean.com/community/tutorial_series/how-to-code-in-go