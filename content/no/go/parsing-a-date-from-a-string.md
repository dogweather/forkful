---
title:                "Analysering av en dato fra en streng"
html_title:           "Go: Analysering av en dato fra en streng"
simple_title:         "Analysering av en dato fra en streng"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?
Å parse en dato fra en streng betyr å konvertere en tekststreng til en spesifikk datoformat. Dette er nyttig for å kunne lese og håndtere datoer i programmene våre, som for eksempel å sortere eller sammenligne dem. 

## Slik gjør man det:
```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Konverter en datostreng til en tid objekt
    t, _ := time.Parse("01/02/2006", "07/20/2021")

    // Bruk t.Format for å formatere datoen
    fmt.Println("Dagens dato er:", t.Format("01 January 2006"))
}
```
**Output:**
```
Dagens dato er: 20 July 2021
```

## Dypdykk:
Dato parsing er en vanlig oppgave for programmerere, spesielt med nyere språk som støtter det direkte som Go. Det finnes også alternative metoder for dato konvertering, som bruk av regex eller tredjeparts biblioteker. I Go, er dato parsing implementert ved hjelp av funksjonen `time.Parse()` som tar inn et layout og en dato string og returnerer et `time` objekt.

## Se også:
- [Dokumentasjon for tidsformat i Go](https://golang.org/pkg/time/#pkg-constants)
- [Alternativ måte å parse en dato i Go ved å bruke regex](https://gobyexample.com/regular-expressions)
- [Tredjeparts bibliotek for dato parsing i Go](https://github.com/araddon/dateparse)