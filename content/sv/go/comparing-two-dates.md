---
title:    "Go: Jämförande av två datum"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför
Att jämföra två datum är en vanlig uppgift när man programmerar. Det kan vara användbart för att utröna skillnaden mellan två datum eller för att se om ett visst datum finns mellan två andra datum.

## Hur man gör
Det första steget i att jämföra två datum i Go är att skapa två `time.Time` variabler med de två datumen som vi vill jämföra. Sedan kan vi använda funktionen `Before()` och `After()` för att se om ett datum kommer före eller efter ett annat.

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Skapa två tidvariabler
	datum1 := time.Date(2021, time.March, 16, 0, 0, 0, 0, time.UTC)
	datum2 := time.Date(2021, time.March, 17, 0, 0, 0, 0, time.UTC)

	// Jämför datumen och skriv ut resultatet
	if datum1.Before(datum2) {
		fmt.Println("Datum 1 kommer före Datum 2")
	} else {
		fmt.Println("Datum 2 kommer före Datum 1")
	}
}
```

Detta kommer att skriva ut "Datum 1 kommer före Datum 2" eftersom 16 mars kommer före 17 mars.

## Deep Dive
För att jämföra mer exakt mellan två datum kan vi använda funktionen `Equal()` som jämför ned till millisekundnivå. Vi kan även använda funktionen `Year()`, `Month()` och `Day()` för att jämföra specifika delar av datumen.

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Skapa två tidvariabler
	datum1 := time.Date(2021, time.March, 16, 0, 0, 0, 0, time.UTC)
	datum2 := time.Date(2021, time.March, 16, 12, 30, 0, 0, time.UTC)

	// Jämför datumen och skriv ut resultatet
	if datum1.Equal(datum2) {
		fmt.Println("Datumen är lika")
	}

	// Jämför år
	if datum1.Year() == datum2.Year() {
		fmt.Println("Åren är lika")
	}
	
	// Jämför månad
	if datum1.Month() == datum2.Month() {
		fmt.Println("Månaderna är lika")
	}
	
	// Jämför dag
	if datum1.Day() == datum2.Day() {
		fmt.Println("Dagarna är lika")
	}
}
```

Detta kommer att skriva ut "Datumen är lika", "Åren är lika", "Månaderna är lika", "Dagarna är lika" eftersom datumen är exakt lika i detta exempel.

## Se även
Här är några resurser som kan vara användbara för att lära sig mer om hur man jämför datum i Go:

- [Go Time Package Documentation](https://golang.org/pkg/time/)
- [Go By Example: Time](https://gobyexample.com/time)
- [StackOverflow: Comparing two dates in Go](https://stackoverflow.com/questions/12118635/comparing-two-dates-in-go)