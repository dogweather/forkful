---
title:    "Go: Utvinne understrenger"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Hvorfor

Å utvinne substringer, eller deler av en streng, er en vanlig oppgave i programmering. Dette kan være nyttig for å få tilgang til spesifikke deler av en tekst, eller for å bearbeide data på en mer effektiv måte. I denne artikkelen vil vi utforske hvordan man kan utføre dette i Go-programmeringsspråket.

## Slik gjør du det

For å utvinne en substring i Go bruker vi funksjonen `strings` og dens `Substring` metode. Denne metoden tar inn to parametere: startindeksen og slutindeksen for den delen av strengen du vil utvinne. La oss se på et eksempel:

```Go
package main

import "fmt"
import "strings"

func main() {
  str := "Dette er en test"

  // utvinne substring fra indeks 5 til 10
  substr := str[5:10]

  fmt.Println("Substring:", substr)
}
```

I dette eksempelet, vil variabelen `substr` inneholde verdien "er en". Ved å definere start- og sluttpunktet for vårt utdrag, kan vi få tilgang til ønsket del av strengen.

## Dypdykk

Det er viktig å merke seg at i Go, gir funksjonen `Substring` oss en *referanse* til delen av strengen, ikke en ny kopi. Dette betyr at eventuelle endringer som gjøres i substrings vil reflekteres i den opprinnelige strengen også.

Videre kan vi også bruke `Substring` metoden på arrayer og slices i Go. Denne funksjonaliteten gjør det mulig å utvinne deler av en liste basert på indekser, på samme måte som med en streng.

## Se Også

- [Golang Documentation - Strings](https://golang.org/pkg/strings/)
- [Substring i Go - stackoverflow](https://stackoverflow.com/questions/19374219/how-to-get-a-substring-in-golang)
- [Golang Tutorial - Hente del av streng](https://golangbot.com/substring/)