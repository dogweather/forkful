---
title:                "Jämför två datum"
html_title:           "Arduino: Jämför två datum"
simple_title:         "Jämför två datum"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att jämföra två datum innebär att bedöma deras kronologiska ordning - vilket är tidigare, samma tidpunkt eller senare. Programmerare gör detta för att utföra åtgärder baserat på datumvärden, som schemaläggning eller tidsspårning.

## Hur gör man:
Använda `Time`-paketet i Go för att jämföra två datum. Här är ett kodexempel: 

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	datum1 := time.Date(2020, 01, 01, 0, 0, 0, 0, time.UTC)
	datum2 := time.Date(2021, 01, 01, 0, 0, 0, 0, time.UTC)

	if datum1.Before(datum2) {
		fmt.Println("Datum1 är före Datum2")
	}

	if datum1.After(datum2) {
		fmt.Println("Datum1 är efter Datum2")
	}

	if datum1.Equal(datum2) {
		fmt.Println("Datum1 är samma som Datum2")
	}
}
```
Ovanstående kod kommer att producera följande utdata:

```
Datum1 är före Datum2
```

## Djupdykning
Att jämföra datum i Go är enkel tack vare designen bakom `Time`-paketet. Innan datumjämförelser blev enklare, var programmerare tvungna att skriva betydligt mer kod för att hantera olika tidszoner, skottår, och så vidare. Alternativen till inbyggda funktioner liknande `Before`, `After` och `Equal` kan inkludera i egna funktioner för att extrahera och jämföra årtal, månad och dag separat. Men detta är inte rekommenderat eftersom det skulle vara mer susceptibelt för fel och kräva mer kod. 

## Se även
För mer information om datum och tider i Go, se följande resurser: 
1. Go Docs: [Time Package](https://golang.org/pkg/time/)
2. GoByExample: [Time](https://gobyexample.com/time) 

Observera att dessa resurser är skrivna på engelska, men de erbjuder detaljerad information om hur du arbetar med tid och datum i Go.