---
title:                "Beräkna ett datum i framtiden eller förflutenheten"
date:                  2024-01-20T17:31:01.933634-07:00
model:                 gpt-4-1106-preview
simple_title:         "Beräkna ett datum i framtiden eller förflutenheten"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Beräkning av datum i framtiden eller förflutet innebär att lägga till eller dra bort tid från ett givet datum. Programmerare gör detta för att hantera bokningar, påminnelser, prenumerationer, eller andra tidsbaserade funktioner i program.

## Hur gör man:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Nuvarande datum och tid
	now := time.Now()
	fmt.Println("Nu:", now)

	// Lägg till 10 dagar
	future := now.AddDate(0, 0, 10)
	fmt.Println("Framtid:", future)

	// Dra bort 10 dagar
	past := now.AddDate(0, 0, -10)
	fmt.Println("Förflutet:", past)
}
```
Exempelutdata:

```
Nu: 2023-04-12 15:04:05.919617 +0200 CEST m=+0.000123456
Framtid: 2023-04-22 15:04:05.919617 +0200 CEST
Förflutet: 2023-04-02 15:04:05.919617 +0200 CEST
```

## Djupdykning

I Go använder vi `time` paketet för datum- och tidshanteringen. Detta paket ger verktyg för att representera och manipulera tid. Beräkning av datum i framtiden eller förflutet är en grundläggande funktion, skapad för att underlätta tidshantering för utvecklare.

Tidigare användes ofta tredjepartspaket eller egna lösningar. Nu är `time` paketet standard och innehåller allt som behövs för de flesta tidsberäkningar.

När man anropar `AddDate` kan negativa värden användas för att backa i tiden, vilket är användbart för historiska data eller återkommande uppgifter.

## Se även:

- Go's officiella dokumentation om `time` paketet: https://pkg.go.dev/time
- Artikel om tidsmanipulation i Go: https://yourbasic.org/golang/time-change-date/
- Go by Example - Datum och tid: https://gobyexample.com/time
