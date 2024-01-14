---
title:    "Go: Sammenligning av to datoer"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Hvorfor

Å sammenligne to datoer kan være en viktig del av programmeringsoppgaver, spesielt når du jobber med tid og datoer. Dette kan være nyttig for å sjekke om en dato kommer før eller etter en annen, eller for å beregne tidsforskjeller.

## Hvordan

For å sammenligne to datoer i Go, kan du bruke funksjonen `Equal()` fra pakken `time`. Denne funksjonen sammenligner to datoer og returnerer en boolsk verdi som viser om de er like eller ikke.

```Go
import "time"

dato1 := time.Date(2020, 1, 1, 0, 0, 0, 0, time.UTC)
dato2 := time.Date(2020, 1, 2, 0, 0, 0, 0, time.UTC)

if dato1.Equal(dato2) {
  println("Datoene er like")
} else {
  println("Datoene er ikke like")
}
```

Output:

```
Datoene er ikke like
```

## Dypdykk

Når du sammenligner to datoer i Go, sammenlignes alle komponentene - år, måned, dag, time, minutter, sekunder og nanosekunder. Dette betyr at selv om to datoer kan være like på et enkelt øyeblikk, for eksempel 1. januar 2020 klokken 00:00:00, kan de fortsatt være forskjellige hvis nanosekundene er ulike. Det er derfor viktig å inkludere alle komponentene når du sammenligner datoer for å få en nøyaktig sammenligning.

## Se også

- [Offisielle dokumentasjon for tids- og datofunksjoner i Go](https://golang.org/pkg/time/)
- [Stack Overflow spørsmål om å sammenligne datoer i Go](https://stackoverflow.com/questions/19731666/compare-two-time-in-golang)