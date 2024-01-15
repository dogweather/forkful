---
title:                "Sammenslåing av strenger"
html_title:           "Go: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å slå sammen strenger, også kjent som å concatenate strings på engelsk, er en viktig del av programmering. Det lar deg kombinere flere tekstelementer og lage nye setninger eller verdier som kan brukes i ditt program. Dette gjør det enklere å lage dynamiske og varierte utdata.

## Hvordan

```Go
package main

import "fmt"

func main() {
  firstName := "Ole"
  lastName := "Nordmann"
  fullName := firstName + " " + lastName
  fmt.Println(fullName)
}
```

Output:
```Go
Ole Nordmann
```

I dette eksempelet bruker vi variabler for å lagre hver del av navnet. Så bruker vi "+" for å kombinere dem og lage en ny variabel med det fulle navnet. Til slutt, bruker vi "fmt" pakken for å skrive ut det fulle navnet til konsollen.

## Deep Dive

Concatenating strings i Go kan gjøres med "+" operatorer eller med "fmt.Sprintf()" funksjonen. Den første metoden er enklere og mer lesbar, mens den andre er mer fleksibel og kan brukes til å lage mer komplekse setninger ved hjelp av formatterspecifiers. Formatterspecifiers lar deg sette inn variabelverdier i en tekststreng på en spesifisert måte.

Det er også viktig å merke seg at i Go, er strenger immutable, som betyr at de ikke kan endres etter at de er opprettet. Derfor vil hver gang du concatenater strenger, vil det faktisk bli opprettet en ny streng.

## Se Også

- [Strings i Go](https://golang.org/pkg/strings/)
- [Formatterspecifiers i Go](https://pkg.go.dev/fmt#pkg-overview)
- [Manipulerbar tilsvarende av strings i Go](https://golang.org/pkg/bytes/)