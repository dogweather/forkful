---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Arduino: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å slette bokstaver som matcher et mønster er en metode å manipulere tekst på i programmering. Det hjelper med å rydde opp uønskede tegn som kan forstyrre dataanalyse eller prosessering.

## Hvordan gjør du det:

Her er et enkelt eksempel på hvordan du fjerner bestemte tegn fra en streng med Go:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	tekst := "Hei, verden!"

	// Fjern kommaer fra strengen
	nyTekst := strings.Replace(tekst, ",", "", -1)
	fmt.Println(nyTekst)
}

```
Koden over vil gi følgende output:

```Go
Hei verden!
```

## Dyp Dykk

I historiske sammenhenger, sletting av tegn som matcher et mønster har vært en standard funksjon i UNIX tekstbehandling verktøy som 'tr' og 'sed'. Da programmeringsspråk utviklet seg, ble denne funksjonaliteten inkorporert direkte i språkets standard biblioteker.

Alternativt, Go tilbyr `Trim`, `TrimLeft`, `TrimRight` funksjoner osv. Disse lar deg fjerne tegn fra begynnelsen og slutten av en streng.

For implementeringsdetaljer i Go, når vi bruker `strings.Replace`, den tredje parameteren bestemmer antall bytter. Hvis det er -1, vil alle forekomster bli erstattet.

## Se også:

1. Go Docs om strenger: https://golang.org/pkg/strings/
2. Go By Example, strenger: https://gobyexample.com/strings