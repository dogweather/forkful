---
title:                "Tolke en dato fra en streng"
date:                  2024-01-20T15:36:19.601303-07:00
html_title:           "Arduino: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Å parse en dato fra en streng betyr å konvertere tekst til et datoformat en datamaskin forstår. Programmerere gjør dette for å håndtere datoer og tider effektivt, som å sammenligne datoer eller lagre dem i databasesystemer.

## How to:
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	const layout = "02-01-2006 15:04:05"
	input := "17-05-2021 13:45:00"
	date, err := time.Parse(layout, input)
	if err != nil {
		fmt.Println("Error parsing date:", err)
		return
	}
	fmt.Println("Parsed date:", date)
}
```
Output:
```
Parsed date: 2021-05-17 13:45:00 +0000 UTC
```

## Deep Dive
Dato-parsing er sentralt i programmering. I Go, formaterer vi datoen etter et referansepunkt kjent som "Mon Jan 2 15:04:05 MST 2006". Viktig å huske er Go's strenge krav til datoformat. Alternativer til Go's innebygde biblioteker inkluderer tredjeparts pakker som `dateparse`, som kan være mer ettergivende med datoformat.

Historisk sett har mange programmer blitt satt ut av spill på grunn av dårlig håndtering av datoer, som Y2K-bug. Dato-parsing hjelper oss forhindre slike problemer og håndtere lokal tid og tidssonekonverteringer.

Implementeringsdetaljer varierer mellom ulike språk. I Go brukes tidspakken og dens Parse-funksjon for å gjøre strenger om til Time-strukturer. Det er viktig å validere og sanitere dato input for å forhindre feil og sikkerhetsproblemer.

## See Also
- Go's time package documentation: https://pkg.go.dev/time
- Date parsing in Go blog post: https://blog.golang.org/parsing-time
- Go by Example: Time formatting/parsing: https://gobyexample.com/time-formatting-parsing
- `dateparse` library for Go: https://github.com/araddon/dateparse
