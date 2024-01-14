---
title:    "Go: Konvertering av dato til en streng"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Hvorfor
Å konvertere en dato til en streng er en vanlig oppgave som ofte er nødvendig i programmering. Dette kan være for å presentere datoer i et mer leselig format for brukeren, eller for å lagre datoer i en database eller fil. I denne bloggposten vil vi se nærmere på hvordan man kan gjøre dette i Go-programmeringsspråket.

## Hvordan gjøre det
Konverteringen av dato til streng i Go kan gjøres ved hjelp av `Format()`-funksjonen fra `time`-pakken. Her er et eksempel på hvordan man kan konvertere dagens dato til en streng i `dd.mm.yyyy`-format:

```Go 
import "fmt"
import "time"

func main() {
    today := time.Now()
    dateString := today.Format("02.01.2006")
    fmt.Println(dateString)
}
```

Kjøringen av dette programmet vil gi følgende output:

```
09.05.2021
```

La oss nå se på hvordan vi kan konvertere en dato fra en variabel til en streng ved hjelp av `Format()`-funksjonen:

```Go
import "fmt"
import "time"

func main() {
    date := time.Date(2021, time.May, 9, 0, 0, 0, 0, time.UTC)
    dateString := date.Format("02-01-2006")
    fmt.Println(dateString)
}
```

Her bruker vi `Date()`-funksjonen fra `time`-pakken til å opprette en `time.Time`-variabel med verdien 9. mai 2021. Deretter konverterer vi denne variabelen til en streng ved hjelp av `Format()`-funksjonen, og får følgende output:

```
09-05-2021
```

## Dypdykk
For å forstå nærmere hvordan `Format()`-funksjonen fungerer, kan vi se på argumentene som tas inn. Første argument er et formatstreng, som består av en kombinasjon av tall og spesielle bokstaver som representerer ulike deler av datoen. På denne måten kan vi selv bestemme hvilket format vi ønsker å få datoen i.

For eksempel vil formatet `"02.01.2006"` gi datoen i formatet `dd.mm.yyyy`, mens formatet `"2006-01-02"` vil gi datoen i formatet `yyyy-mm-dd`. De spesielle bokstavene er definert i dokumentasjonen for `time`-pakken og kan kombineres på ulike måter for å få ønsket format.

## Se også
- [Golang.org - Package time](https://golang.org/pkg/time/)
- [A tour of Go - Time formatting](https://tour.golang.org/basics/15)