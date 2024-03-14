---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:00:17.743137-07:00
description: "Het parsen van een datum uit een string in Go houdt in dat de als tekst\
  \ weergegeven datum wordt omgezet naar een bruikbaarder formaat (bijv. `time.Time`).\u2026"
lastmod: '2024-03-13T22:44:50.297382-06:00'
model: gpt-4-0125-preview
summary: "Het parsen van een datum uit een string in Go houdt in dat de als tekst\
  \ weergegeven datum wordt omgezet naar een bruikbaarder formaat (bijv. `time.Time`).\u2026"
title: Een datum ontleden uit een string
---

{{< edit_this_page >}}

## Wat & Waarom?

Het parsen van een datum uit een string in Go houdt in dat de als tekst weergegeven datum wordt omgezet naar een bruikbaarder formaat (bijv. `time.Time`). Programmeurs voeren deze taak uit om datum- en tijdgegevens nauwkeuriger te hanteren in applicaties, vooral wanneer ze te maken hebben met gebruikersinvoer, API's of opslagsystemen waar datums vaak als strings worden weergegeven.

## Hoe te:

Go biedt robuuste ondersteuning voor het parsen van datums en tijden door middel van het `time`-pakket. De sleutel is het begrijpen van Go's referentiedatumformaat: `Mon Jan 2 15:04:05 MST 2006`, dat je gebruikt om Go te vertellen hoe de binnenkomende string te interpreteren. Hier is een snel voorbeeld om je op weg te helpen:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Voorbeeld datum string
	dateStr := "2023-04-12 14:45:00"
	
	// Definieer de lay-out/het formaat van de invoerdatumstring
	// Deze lay-out vertelt Go om een jaar te verwachten, gevolgd door een maand, 
	// dan een dag, uur, minuut en ten slotte seconde
	layout := "2006-01-02 15:04:05"
	
	// Parse de datumstring volgens de lay-out
	parsedDate, err := time.Parse(layout, dateStr)
	if err != nil {
		fmt.Println("Fout bij het parsen van de datum:", err)
		return
	}
	
	// Toon de geparseerde datum
	fmt.Println("Geparste Datum:", parsedDate)
}
```

Wanneer je deze code uitvoert, krijg je:

```
Geparste Datum: 2023-04-12 14:45:00 +0000 UTC
```

Let op hoe de `layout`-string de waarden van de referentiedatum gebruikt om het formaat van de invoerstring te specificeren. Pas de `layout` aan om overeen te komen met het formaat van je invoerdatums.

## Diepgaande Duik

Het ontwerp van Go's datum- en tijdsparsing is uniek, gebruik makend van een specifieke referentiedatum (`Mon Jan 2 15:04:05 MST 2006`). Deze benadering, in plaats van meer conventionele formaatspecifiers te gebruiken (zoals `YYYY` voor jaar), is gekozen voor leesbaarheid en gebruiksgemak, waarbij een meer op voorbeelden gebaseerd formaat wordt benut.

Hoewel dit aanvankelijk ongebruikelijk kan lijken voor programmeurs die gewend zijn aan andere talen, vinden velen het na een korte aanpassingsperiode intuïtiever. Voor applicaties die complexere datummanipulatie vereisen of formaten die niet direct door Go's `time`-pakket worden ondersteund, kunnen externe bibliotheken zoals `github.com/jinzhu/now` extra functionaliteit bieden. Echter, voor de meerderheid van de standaardtoepassingen zijn de ingebouwde mogelijkheden van Go robuust, performant en idiomatic, belichamend de Go-filosofie van eenvoud en duidelijkheid.
