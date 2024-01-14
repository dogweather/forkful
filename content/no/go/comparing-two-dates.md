---
title:                "Go: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor
Å sammenligne to datoer er en vanlig oppgave for utviklere når de søker å håndtere tid og datoer i programmering. Ved å sammenligne to datoer kan vi utføre logikk og ta beslutninger basert på hvilken dato som kommer før eller etter en annen. Dette er spesielt nyttig i applikasjoner som involverer planlegging og frister.

## Slik gjør du det
For å sammenligne to datoer i Go språket trenger vi å bruke funksjonen ```Equality()``` fra ```time``` pakken. Vi kan bruke denne funksjonen til å sammenligne to datoer og få tilbake en sann eller usann verdi, avhengig av om de er like eller ikke. Her er et eksempel på hvordan vi kan implementere dette i koden vår:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	date1 := time.Date(2020, time.November, 15, 0, 0, 0, 0, time.UTC)
	date2 := time.Date(2020, time.December, 1, 0, 0, 0, 0, time.UTC)

	if date1.Equal(date2) {
		fmt.Println("Datoene er like.")
	} else {
		fmt.Println("Datoene er ikke like.")
	}
}
```

Output av denne koden vil være "Datoene er ikke like.", siden de to datoene som er definert i koden er forskjellige. Vi kan også bruke andre metoder som ```Before()``` og ```After()``` for å sammenligne datoer i forhold til hverandre.

## Dypdykk
Det er viktig å være klar over hvordan Go håndterer tid og datoer, spesielt når det kommer til tidszoner og sommertid. Go bruker internasjonale standarder for tid og dato håndtering, som kan føre til uventede resultater når man sammenligner datoer fra ulike tidszoner eller når sommertid er aktivert.

En annen viktig ting å merke seg er at Go bruker UTC (Coordinated Universal Time) som standard tidszone. Dette kan føre til problemer hvis man ikke er klar over det, spesielt når man jobber med brukerspesifikke tidszoner.

## Se også
- [Offisiell Go dokumentasjon for time pakken](https://golang.org/pkg/time/)
- [Sammenligning av datoer i andre programmeringsspråk](https://www.baeldung.com/java-compare-dates)
- [Vanlig feil når man arbeider med tid og dato i programmering](https://weblogs.asp.net/dwahlin/common-mistakes-when-working-with-dates-in-javascript)