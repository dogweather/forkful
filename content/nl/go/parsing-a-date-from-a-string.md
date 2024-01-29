---
title:                "Een datum uit een string parsen"
date:                  2024-01-28T22:04:04.908909-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum uit een string parsen"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het parsen van een datum houdt in dat je een string omzet in een datumobject. Programmeurs doen dit om datums in een gestandaardiseerde vorm te kunnen hanteren voor opslag, sortering of manipulatie.

## Hoe:
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Voorbeeld datum string
	dateStr := "2023-04-01T15:04:05Z"

	// Definieer layout overeenkomend met het bovenstaande formaat
	layout := time.RFC3339

	// Parse string naar time.Time object
	parsedDate, err := time.Parse(layout, dateStr)
	if err != nil {
		panic(err)
	}
	
	// Uitvoer
	fmt.Printf("Geparste Datum: %v\n", parsedDate)
}
```
Uitvoer:
```
Geparste Datum: 2023-04-01 15:04:05 +0000 UTC
```

## Diepgaand
Het parsen van datums is altijd cruciaal geweest voor softwaresystemen om tijdelijke informatie te organiseren en te verwerken, sinds, nou ja, vrijwel het begin van de computerwetenschap. In Go vertegenwoordigt `time.Time` de structuur die tijd voorstelt. Het is ontworpen voor eenvoud en efficiëntie. Waarom beginnen met strings? Voornamelijk omdat datums als tekst uit verschillende bronnen komen, zoals API's of gebruikersinvoer.

Alternatieven? Wel, je zou theoretisch handmatig kunnen parsen, maar dat is gevoelig voor fouten. De functie `time.Parse` van Go laat je een layout definiëren – een referentiedatum – die je vergelijkt met je invoer. Het is een robuuste methode omdat, sinds Go 1 (circa 2012), deze psychologische meetwaarde voor menselijk leesbare tijd je parsing op het juiste spoor houdt. Python's `datetime` en Java's `SimpleDateFormat` bieden soortgelijke functionaliteit, maar ze zijn niet zo strikt als Go's parse-implementatie, die niet probeert te raden wat je bedoelde.

Hier komt het interessante: de parse-functie van Go heeft een specifieke referentietijd nodig: `Mon Jan 2 15:04:05 MST 2006` (01/02 03:04:05PM '06 -0700). Onthoud deze exacte volgorde; veel mensen doen dat aan de hand van de ezelsbrug, "1 2 3 4 5 6 7".

## Zie Ook
- Go by Example: Tijd Formattering/Parsing: https://gobyexample.com/time-formatting-parsing
- Go tijd pakket doc: https://pkg.go.dev/time#Parse
- The Go Blog over Tijd: https://blog.golang.org/time
