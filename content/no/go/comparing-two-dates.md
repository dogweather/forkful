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

## Hvorfor

Å sammenligne to datoer kan være nyttig for å avgjøre hvilken dato som er tidligere eller senere, eller for å se hvor lenge det har gått mellom to hendelser.

## Hvordan

Sammenligning av datoer kan gjøres ved hjelp av Go sin innebygde tidsfunksjonalitet. Ved å bruke funksjoner som `time.Date()`, `time.Before()`og `time.After()`, kan vi enkelt sammenligne to datoer.

```Go
// Lage to tidspunker
tid1 := time.Date(2021, time.January, 10, 0, 0, 0, 0, time.UTC)
tid2 := time.Date(2021, time.February, 1, 0, 0, 0, 0, time.UTC)

// Sammenligne hvilken tid som er først og hvilken som er sist
if tid1.Before(tid2) {
    fmt.Println("Første tid er tidligere enn andre tid")
}

// Se hvor lang tid det er gått mellom to tidspunker
diff := tid2.Sub(tid1)
fmt.Println("Det har gått", diff, "mellom de to tidene.")
```

Output:
```
Første tid er tidligere enn andre tid
Det har gått 22h0m0s mellom de to tidene.
```

## Dypdykk

Når man sammenligner datoer, bør man være oppmerksom på at tider må være i samme tidsone for å få riktig resultat. Man kan også bruke funksjonen `time.Equal()` for å sjekke om to tider er helt like.

Se også på Go sin dokumentasjon for mer informasjon om tidsfunksjonaliteten: https://golang.org/pkg/time/

## Se også

- Mer informasjon om Go sin tidsfunksjonalitet: https://golang.org/pkg/time/
- En guide for å komme i gang med Go-programmering: https://gobyexample.com/
- En online lekeplass for å teste og øve på Go-kode: https://play.golang.org/