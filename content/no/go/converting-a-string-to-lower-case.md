---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Go: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange ganger i programmering må vi håndtere tekststrenger og det er viktig å kunne endre dem til en spesifikk form. Å konvertere en string til lower case er nyttig for å sikre at betingelsene eller søkene våre er mer nøyaktige og for å sikre en konsekvent standardisering av tekststrenger. Dette kan være spesielt nyttig når vi jobber med brukerinput eller når vi sammenligner strenger for å unngå feil.

## Hvordan gjøre det

For å konvertere en string til lower case i Go, kan vi bruke innebygde funksjoner som "ToLower" fra "strings" pakken. Her er et eksempel på hvordan vi gjør det:

```Go
package main 

import (
  "fmt"
  "strings"
)

func main() {
  str := "HELLO WORLD"
  lowerCaseStr := strings.ToLower(str)
  fmt.Println(lowerCaseStr)
}
```

Output:

hello world

Som du kan se, bruker vi først "strings" pakken og deretter funksjonen "ToLower" på en string-variabel. Vi tilordner resultatet av denne funksjonen til en ny variabel og skriver ut den nye variabelen som inneholder den konverterte stringen i lower case.

## Dypdykk

Det er verdt å merke seg at når vi bruker "strings.ToLower" funksjonen, blir den konverterte stringen en ny string-variabel, og den opprinnelige variabelen forblir uendret. Hvis vi ønsker å endre den opprinnelige variabelen, kan vi bruke "strings.ToLower" funksjonen og tilordne resultatet direkte til den opprinnelige variabelen.

Vi kan også bruke "ToLowerSpecial" funksjonen fra "unicode" pakken hvis vi ønsker å ta hensyn til spesielle tegn og språkkoder i stringen vår. Denne funksjonen tar inn en "case mapping" funksjon som et argument og brukes til å konvertere stringen.

## Se også

- [Go strings pakke dokumentasjon](https://golang.org/pkg/strings/)
- [Go unicode pakke dokumentasjon](https://golang.org/pkg/unicode/)