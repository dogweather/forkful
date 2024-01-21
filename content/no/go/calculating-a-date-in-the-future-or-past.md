---
title:                "Beregning av en dato i fremtiden eller fortiden"
date:                  2024-01-20T17:31:03.900235-07:00
model:                 gpt-4-1106-preview
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Beregning av en dato i fremtiden eller fortiden handler om å legge til eller trekke fra tid fra en gitt dato. Programmerere gjør dette for å håndtere frister, planlegging, tidsbaserte funksjoner eller rett og slett for å finne tidsdifferansen mellom to datoer. 

## Hvordan gjøre:
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Nåværende tidspunkt
	now := time.Now()
	fmt.Println("Nå: ", now)

	// Legg til to dager
	twoDaysLater := now.AddDate(0, 0, 2)
	fmt.Println("To dager senere: ", twoDaysLater)

	// Trekk fra tre uker
	threeWeeksBefore := now.AddDate(0, 0, -21)
	fmt.Println("Tre uker tidligere: ", threeWeeksBefore)

	// Formater dato til lokal standard
	fmt.Println("Formatert dato: ", twoDaysLater.Format("02.01.2006"))
}
```
Sample output:
```
Nå:  2023-04-12 15:04:05.999999 +0200 CEST
To dager senere:  2023-04-14 15:04:05.999999 +0200 CEST
Tre uker tidligere:  2023-03-22 15:04:05.999999 +0200 CEST
Formatert dato:  14.04.2023
```

## Dypdykk
Metoden `AddDate` i `time`-pakken er Go sin integrerte måte å håndtere datoarithmetikk på. Historisk sett har håndtering av tid vært komplisert grunnet forskjeller i tidssoner, skuddår og andre kalenderregler. Alternativer inkluderer tidshåndtering med UNIX-tidstempel og innebygde funksjoner i databaser. Når det kommer til Go, så er `time`-pakken konstruert for å gjøre tidshåndtering så rett frem som mulig, samtidig som den tar høyde for kompleksiteten i virkelighetens tidshåndtering. 

## Se også
- Go's `time` pakke dokumentasjon: https://pkg.go.dev/time
- UTF-8 Rune (tegn) i Golang: https://blog.golang.org/strings
- Effektiv håndtering av tidsintervaller i Go: https://yourbasic.org/golang/time-change-month-day-hour/
- Go by Example: Time: https://gobyexample.com/time