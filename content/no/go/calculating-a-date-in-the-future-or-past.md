---
title:    "Go: Beregning av en dato i fremtiden eller fortiden"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Hvorfor
Å kunne beregne en dato i fremtiden eller fortiden kan være nyttig for å planlegge hendelser eller for å håndtere datoer i en applikasjon. Go har innebygde funksjoner for å hjelpe deg med å beregne datoer på en enkel måte.

## Hvordan
For å beregne en dato i fremtiden eller fortiden i Go, kan du bruke funksjonen "time.Date". Denne funksjonen tar inn år, måned og dag som argumenter, og returnerer en "time.Time" variabel som representerer den valgte datoen. La oss se på et eksempel:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Beregner en dato 10 dager frem i tid
    futureDate := time.Date(2021, time.November, 10, 0, 0, 0, 0, time.UTC)
    fmt.Println("Dato i fremtiden:", futureDate)

    // Beregner en dato 5 år tilbake i tid
    pastDate := time.Date(2016, time.July, 15, 0, 0, 0, 0, time.UTC)
    fmt.Println("Dato i fortiden:", pastDate)
}
```
Dette vil gi følgende utskrift:

```
Dato i fremtiden: 2021-11-10 00:00:00 +0000 UTC
Dato i fortiden: 2016-07-15 00:00:00 +0000 UTC
```

## Dykk dypere
Funksjonen "time.Date" kan også ta inn en "time.Location" variabel som bestemmer tids- og tidssoneinnstillingene for den beregnede datoen. Du kan også bruke funksjonene "AddDate" og "Sub" for å legge til eller trekke fra tidsenheter som år, måneder, dager, timer, osv. fra en gitt dato.

## Se også
- [Offisiell Go dokumentasjon for time.Date](https://golang.org/pkg/time/#Date)
- [Tutorial om å arbeide med datoer og tid i Go](https://learning.oreilly.com/library/view/learning-go/9781785889639/ch05s04.html)
- [Gode praksiser for å håndtere datoer i Go](https://stackoverflow.com/questions/36530251/how-to-cope-with-go-time-crap)