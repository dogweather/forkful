---
title:                "Go: Konvertering av en dato til en streng"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å konvertere en dato til en streng er en vanlig oppgave i programmering og kan være nyttig av flere grunner, både for å kunne presentere datoen på en mer lesbar måte og for å kunne lagre den i en spesifikk format.

## Slik gjør du det

For å konvertere en dato til en streng i Go programmeringsspråket, kan du bruke "```Go date.Format() ```" funksjonen. Denne funksjonen tar inn et format som parameter og returnerer datoen i en streng som er formatert i henhold til dette formatet.

Her er et eksempel som viser hvordan du kan konvertere en dato til en streng i ulike formater:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	date := time.Date(2021, time.October, 15, 0, 0, 0, 0, time.UTC)

	fmt.Println(date.Format("02-01-2006")) // Output: 15-10-2021
	fmt.Println(date.Format("January 2, 2006")) // Output: October 15, 2021
	fmt.Println(date.Format("06-01-02")) // Output: 21-10-15
}
```

Som du kan se, kan du enkelt formatere datoen i henhold til ditt behov ved å endre parameteret i "```Go date.Format() ```" funksjonen.

## Dype dykk

For å kunne formatere datoen korrekt, er det viktig å forstå de ulike formatene som brukes i "```Go date.Format() ```" funksjonen. Noen av de vanligste formatene inkluderer:

- "01" og "02": Brukes for å representere måneder og dager med to siffer. For eksempel vil "01" representere januar og "02" vil representere andre dag i måneden.
- "2006": Brukes for å representere år med fire siffer. Dette kan virke merkelig, men det er en del av standarden for hvordan datoen skal formateres i Go.
- "15" og "03": Brukes for å representere timer og minutter med to siffer. For eksempel vil "15" representere klokken 15 og "03" vil representere tre minutter.
- "Monday" og "01/02/2006": Brukes for å representere ukedager og datoer i en mer lesbar format. For eksempel vil "Monday" representere den ukedagen som datoen faller på, og "01/02/2006" vil representere måned og dag i måneden.

Ved å kombinere disse formatene og endre rekkefølgen på dem, kan du formatere datoen på en måte som passer best for din applikasjon.

## Se også

- [Go's time package](https://golang.org/pkg/time/)
- [Official Go documentation on date formatting](https://golang.org/pkg/time/#Time.Format)