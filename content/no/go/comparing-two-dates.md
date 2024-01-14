---
title:                "Go: Sammenligning av to datoer"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Hvorfor

Du har kanskje stått i en situasjon der du trenger å sammenligne to datoer i et Go-program, enten det er for å finne ut hvilken dato som kommer først eller for å sjekke om to datoer er like. Uansett årsak, ved å forstå hvordan man sammenlikner datoer i Go, kan du effektivt håndtere datoer i dine programmer.

# Slik gjør du det

For å sammenligne to datoer i Go, kan du bruke funksjonen `.Before()` eller `.Equal()` fra pakken `"time"`. La oss se på et eksempel for å sammenligne to datoer i en enkel Go-applikasjon:

```
package main

import (
  "fmt"
  "time"
)

func main() {
  date1 := time.Date(2020, time.November, 10, 0, 0, 0, 0, time.UTC)
  date2 := time.Date(2020, time.October, 15, 0, 0, 0, 0, time.UTC)

  if date1.Before(date2) {
    fmt.Println("Date 1 comes before Date 2")
  }

  if date1.After(date2) {
    fmt.Println("Date 1 comes after Date 2")
  }

  if date1.Equal(date2) {
    fmt.Println("Date 1 and Date 2 are equal")
  }
}
```

Her har vi definert to datoer, `date1` og `date2`, og brukt `.Before()`, `.After()` og `.Equal()` funksjonene for å sammenligne dem. Hvis du kjører dette programmet, vil du se at `Date 1 comes before Date 2` blir skrevet ut, siden oktober kommer før november. Du kan også endre datoene og se hvordan konsollet endrer utskriften.

# Dypdykk

Når du sammenligner datoer i Go, er det viktig å merke seg at datatypen `time.Time` er en struktur som inneholder informasjon om både tid og dato. Derfor må du være nøyaktig når du definerer datoene dine, ved å angi timer, minutter, sekunder og tidssone.

En annen ting å huske på er at `.Equal()` funksjonen bare sjekker om to datoer er identiske, inkludert tidssonen. Hvis du vil sjekke om to datoer er like uten å ta hensyn til tidssonen, kan du bruke `.Truncate()` funksjonen for å nullstille tidssonen før du sammenligner.

# Se også

* [Go's `time` pakke dokumentasjon](https://golang.org/pkg/time/)
* [Sammenligne datoer og tider i Go - Tutorialspoint](https://www.tutorialspoint.com/go/go_compare_dates.htm)
* [Comparing dates in Go - Stack Overflow](https://stackoverflow.com/questions/56293519/how-to-compare-dates-in-golang)