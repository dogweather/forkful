---
title:                "Beräkning av ett datum i framtiden eller det förflutna"
html_title:           "Go: Beräkning av ett datum i framtiden eller det förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller det förflutna"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför
Att kunna beräkna ett datum i framtiden eller i det förflutna är en vanlig uppgift inom programmering. Det kan vara användbart för att till exempel skapa en almanacka eller för att hantera återkommande händelser.

## Så här gör du
För att beräkna ett datum i framtiden eller förflutna i Go, kan du använda tids- och datumfunktionerna som finns inbyggda i språket.

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Beräkning av ett datum i framtiden
	futureDate := time.Now().AddDate(1, 0, 0) // lägger till ett år från nu
	fmt.Println(futureDate)

	// Beräkning av ett datum i förflutna
	pastDate := time.Now().AddDate(-1, 0, 0) // drar bort ett år från nu
	fmt.Println(pastDate)
}
```

Output:

```Go
2020-05-20  // datumet ett år framåt i tiden
2018-05-20  // datumet ett år bakåt i tiden
```

## Djupdykning
För att få en djupare förståelse för hur beräkning av datum i framtiden eller förflutna fungerar i Go, kan vi titta på time-paketet. Funktionen `AddDate(years, months, days)` tar in tre parametrar och returnerar ett nytt tid- och datumobjekt. Detta gör det enkelt att manipulera tid och datum enligt våra behov.

## Se även
- [The Go Programming Language](https://golang.org/)
- [Time Package] (https://golang.org/pkg/time/)
- [Go Playground] (https://play.golang.org/)