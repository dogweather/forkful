---
title:                "Sammenligning av to datoer"
html_title:           "Go: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Sammenligning av to datoer er en vanlig oppgave for programmerere når de arbeider med dato og tidsfunksjonalitet. Dette gjøres ofte for å sjekke om en dato kommer før eller etter en annen, eller om de er like.

## Hvordan:

For å sammenligne to datoer i Go, kan vi bruke Compare-funksjonen fra "time" biblioteket. Denne funksjonen sammenligner to tidsobjekter og returnerer en int verdi. En positiv verdi betyr at den første datoen kommer etter den andre, en negativ verdi betyr at den kommer før, og 0 betyr at de er like. Se eksemplet nedenfor:

```
package main

import (
    "fmt"
    "time"
)

func main() {
    dato1 := time.Date(2020, time.December, 1, 12, 0, 0, 0, time.UTC)
    dato2 := time.Date(2021, time.January, 1, 12, 0, 0, 0, time.UTC)

    sammenligning := time.Compare(dato1, dato2)

    fmt.Println(sammenligning)
}
```

Output: -1 

Dette betyr at dato1 kommer før dato2.

## Dypdykk:

Å sammenligne datoer har vært en utfordring for programmerere i lang tid. I eldre programmeringsspråk måtte man konvertere datoer til sekunder eller minutter og deretter sammenligne disse verdiene. Go gjør det enklere for oss ved å tilby en dedikert Compare-funksjon.

Det finnes også andre alternativer for å sammenligne datoer, for eksempel ved hjelp av "==", "<" eller ">" operatørene. Men disse vil bare sammenligne de visuelle verdiene av datoene og ikke ta hensyn til tidssoner eller formater.

Når det kommer til implementasjonen av Compare-funksjonen, bruker Go internasjonale standarder for å sikre nøyaktighet og korrekt sammenligning av datoer.

## Se også:

* [https://golang.org/pkg/time/#Date](https://golang.org/pkg/time/#Date) - Offisiell dokumentasjon for Date-funksjonen i Go.
* [https://www.arkondata.com/formatting-dates-time-go/](https://www.arkondata.com/formatting-dates-time-go/) - En tutorial om formatering av dato og tid i Go.