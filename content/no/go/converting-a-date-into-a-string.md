---
title:                "Go: Konvertere en dato til en streng."
simple_title:         "Konvertere en dato til en streng."
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Å konvertere en dato til en streng kan være en nyttig funksjon i Go-programmering for å vise datoer på en mer lesbar måte. Dette kan være spesielt nyttig når du arbeider med brukergrensesnitt eller når du trenger å lagre datoer i en database.

## Hvordan gjør man det
For å konvertere en dato til en streng i Go, kan du bruke `Time` og `Format`-metodene. Først må du opprette en `Time`-variabel som inneholder dato og tidspunktet du vil konvertere. Deretter kan du bruke `Format`-metoden og angi ønsket format for datoen.

```Go
import "fmt"
import "time"

func main() {
    t := time.Date(2021, time.November, 20, 12, 0, 0, 0, time.UTC)
    fmt.Println(t.Format("02.01.2006"))
}
```
Dette vil produsere følgende output: `20.11.2021`, som er den ønskede datoen som en streng. Du kan også tilpasse formatet ved å bruke ulike formateringstegn, som beskrevet i Go sin offisielle dokumentasjon.

## Dypdykk
Det er viktig å merke seg at Go bruker en spesifikk datoformat i sin `Format`-metode, kalt "reference time". Dette er `02.01.2006 15:04:05 MST`, som er en fastsatt dato og tidspunkt som brukes som referanse for å formatere andre datoer. Dette gjør at Go kan tydelig definere formatteringstegnene og produsere korrekt output.

I tillegg er det verdt å nevne at du også kan konvertere en dato til en streng ved hjelp av `strconv`-pakken i Go. Dette kan være nyttig når du jobber med mer komplekse datoformater eller når du trenger å konvertere datoen til en annen format, for eksempel fra en amerikansk til en europeisk datoformat.

## Se også
- [Offisiell dokumentasjon for Go sin "Time" og "Format" metode] (https://pkg.go.dev/time#pkg-overview)
- [Go sin offisielle dokumentasjon for tidformatering] (https://golang.org/pkg/time/#pkg-constants)