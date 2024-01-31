---
title:                "Een string met hoofdletters maken"
date:                  2024-01-28T21:56:04.988056-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string met hoofdletters maken"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het kapitaliseren van een string verandert de eerste letter van een gegeven string in een hoofdletter. Programmeurs doen dit voor het formatteren van uitvoer, het naleven van grammaticale regels, of het leesbaarder maken van tekst.

## Hoe:
In Go zijn strings onveranderlijk, dus je moet een nieuwe gekapitaliseerde versie van de string creëren. We gebruiken het `strings` pakket en zijn `Title` functie of manipuleren de string-runen direct:

```Go
package main

import (
	"fmt"
	"strings"
	"unicode"
)

func main() {
	// Voorbeeld 1: Gebruik van strings.Title om elk woord te kapitaliseren
	fmt.Println(strings.Title("hallo wereld!")) // Geeft als output: Hallo Wereld!

	// Voorbeeld 2: Alleen de eerste karakter kapitaliseren
	input := "hallo opnieuw!"
	if len(input) > 0 {
		fmt.Println(strings.ToUpper(string(input[0])) + input[1:]) // Geeft als output: Hallo opnieuw!
	}

	// Voorbeeld 3: Meer robuuste kapitalisatie, behandelt multi-byte karakters
	capitalizeFirst := func(s string) string {
		for i, v := range s {
			return string(unicode.ToUpper(v)) + s[i+utf8.RuneLen(v):]
		}
		return ""
	}

	fmt.Println(capitalizeFirst("привет мир!")) // Geeft als output: Привет мир!
}
```

## Uitdieping
Stringkapitalisatie is geen ingewikkeld proces, maar er gebeurt veel onder de motorkap. Voordat de `strings.Title` functie bestond, moest je strings op het niveau van runen manipuleren voor een correcte kapitalisatie.

In oudere programmeertalen was het lastig om niet-ASCII karakters te behandelen tijdens het kapitaliseren door het gebrek aan goede Unicode-ondersteuning. Go maakt het makkelijker met ingebouwde ondersteuning voor UTF-8 codering in de `unicode` en `utf8` standaardpakketten.

Wanneer je handmatig strings kapitaliseert in Go, vergeet dan niet om multi-byte karakters te behandelen. Daarom lopen we door de string heen met `range` in het robuuste voorbeeld, wat itereert over runen in plaats van bytes.

Er zijn alternatieven voor de ingebouwde Go-methoden, zoals het gebruik van externe bibliotheken voor complexere tekstmanipulatiebehoeften. Echter, voor eenvoudige kapitalisatie is de standaardbibliotheek van Go meestal voldoende.

## Zie Ook
- Go strings pakket: [https://golang.org/pkg/strings/](https://golang.org/pkg/strings/)
- Go unicode pakket: [https://golang.org/pkg/unicode/](https://golang.org/pkg/unicode/)
- Go utf8 pakket: [https://golang.org/pkg/unicode/utf8/](https://golang.org/pkg/unicode/utf8/)
- Een cool artikel over strings en runen in Go: [https://blog.golang.org/strings](https://blog.golang.org/strings)
