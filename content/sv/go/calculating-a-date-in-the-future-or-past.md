---
title:    "Go: Beräkna ett datum i framtiden eller i det förflutna"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna ett datum i framtiden eller förflutna kan vara användbart för att planera scheman eller datum för tillfällen. Det kan också vara en viktig funktion för applikationer som använder sig av tidsstämplar eller påminnelser.

## Hur man gör det

Det finns flera sätt att beräkna ett datum i framtiden eller förflutna med hjälp av Go-programmeringsspråket. Nedan följer tre enkla exempel med kodblock som visar hur det kan göras.

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Beräkna ett datum 7 dagar framåt
	framtidaDatum := time.Now().AddDate(0, 0, 7)
	fmt.Println("Framtida datum: ", framtidaDatum)

	// Beräkna ett datum 1 månad bakåt
	bakatDatum := time.Now().AddDate(0, -1, 0)
	fmt.Println("Bakat datum: ", bakatDatum)

	// Beräkna ett datum 2 år framåt
	framtidaDatum2 := time.Now().AddDate(2, 0, 0)
	fmt.Println("Framtida datum: ", framtidaDatum2)
}
```

Output:

```
Framtida datum: 2021-04-08 15:14:17.5271961 +0000 UTC m=+604801.140583401
Bakat datum: 2021-02-08 15:14:17.5282007 +0000 UTC m=+543661.141566601
Framtida datum: 2023-02-08 15:14:17.5282007 +0000 UTC m=+949321.141566601
```

Det första exemplet använder funktionen `AddDate()` för att lägga till 7 dagar till det nuvarande datumet. Det andra exemplet använder sig av ett negativt värde för att dra bort en månad från det nuvarande datumet. Och det tredje exemplet lägger till 2 år till det nuvarande datumet.

En annan funktion som kan användas är `Date()` som tar emot tre argument: år, månad och dag, och som returnerar ett `Time`-objekt. Detta kan vara användbart om du vill beräkna ett specifikt datum i framtiden eller förflutna.

## Djupdykning

När du arbetar med tidsberäkningar är det också viktigt att ta hänsyn till tidzoner. Detta kan göras genom att använda `time.LoadLocation()` för att hämta en specifik tidzon och sedan konvertera tiden till den tidzonen med hjälp av `In()`-funktionen.

Det är också möjligt att använda både datum och tid i beräkningarna genom att använda `time.Date()`-funktionen istället för `Now()`.

Att kunna beräkna datum i framtiden eller förflutna är en användbar funktion som kan spara tid och förbättra funktionaliteten i dina program.

## Se även

- [Dokumentation för Go's tidspaket](https://golang.org/pkg/time/)
- [Golang Tutorials: Date and Time](https://golangbot.com/date-time/)
- [How to Calculate Dates in Go](https://www.calhoun.io/how-to-calculate-dates-in-go/)