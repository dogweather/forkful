---
title:                "Utskrift av feilsøkingsresultat"
html_title:           "Arduino: Utskrift av feilsøkingsresultat"
simple_title:         "Utskrift av feilsøkingsresultat"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Utskrift av feilsøkingsdata er prosessen med å vise interne verdier fra koden vår under kjøring. Vi gjør det for å forstå hva som skjer inne i programmet vårt når vi feilsøker eller tester det.

## Hvordan:
Her er et enkelt eksempel på hvordan man skriver ut feilsøkingsdata i Go:

```Go
package main

import "fmt"

func main(){
    a := 5;
    b := 7;
    sum := a + b;
    
    fmt.Printf("Summen av %d og %d er %d\n", a, b, sum)
}
```

Når du kjører denne koden, vil utskriften være: 

```
Summen av 5 og 7 er 12
```

## Dypdykk:
Utskrift av feilsøkingsdata har vært en del av programmering siden de første datamaskinene. Selv om det finnes mer avanserte feilsøkingsverktøy i dag, er denne metoden fortsatt nyttig for dets enkelhet.

Dykker vi dypere inn, ser vi at Go bruker `fmt` pakken for utskrift. Denne pakken inneholder mange funksjoner, men `fmt.Print()`, `fmt.Println()` og `fmt.Printf()` er mest brukt.

Alternativer til `fmt` pakken innebærer bruk av tredjeparts biblioteker som logrus eller zap som tilbyr mer komplekse logging funksjoner. 

Når vi kjører `fmt.Printf()`, formatterer Go teksten før den skriver den til standard utdatastrøm. Det gjør det mulig å inkludere variable verdier direkte i utskriftsstringene våre.

## Se Også:
Her er noen nyttige lenker for å lære mer om Go og utskrift av feilsøkingsdata:

- Offisiell Go Dokumentasjon: https://golang.org/doc/
- fmt pakken: https://golang.org/pkg/fmt/
- logrus, et kraftig loggbibliotek for Go: https://github.com/sirupsen/logrus
- zap, et annet raske logging bibliotek for Go: https://github.com/uber-go/zap