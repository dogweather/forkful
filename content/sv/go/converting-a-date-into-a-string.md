---
title:    "Go: Omvandling av ett datum till en sträng"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Varför

Att konvertera en datum till en sträng är en vanlig funktion i många typer av program. Det är användbart för att kunna visa datum i en läsbar form eller för att bearbeta datum på ett mer effektivt sätt.

## Så här gör du

För att konvertera ett datum till en sträng i Go, använder vi funktionen `Format()` från paketet `time`. Det finns flera olika formatmallar som kan användas för att välja hur datumet ska visas, men för enkelhetens skull använder vi den vanligaste mallen "Jan 2 15:04:05 MST 2006" i detta exempel.

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	datum := time.Date(2021, time.February, 14, 18, 30, 0, 0, time.UTC)
	datumStr := datum.Format("Jan 2 15:04:05 MST 2006")
	fmt.Println(datumStr) // Output: Feb 14 18:30:00 UTC 2021
}
```

Som vi kan se i exemplet ovan används `Format()` funktionen för att konvertera datumet `2021-02-14T18:30:00Z` till strängen "Feb 14 18:30:00 UTC 2021".

## Djupdykning

I Go är datum- och tidsåtgärder baserade på det inbyggda paketet `time`. Detta paket innehåller funktioner för att arbeta med tid, tidszoner och datumkonverteringar. För att konvertera ett datum från en sträng till en `time.Time`-typ, kan vi använda funktionen `Parse()`. Den kan läsa in en sträng i ett givet format och returnera ett `time.Time`-objekt.

Andra vanliga formatmallar som kan användas i `Format()`-funktionen inkluderar `RFC3339`, som används för att representera tider enligt RFC3339-standarden, och `UnixDate`, som används för att representera en tid som ett Unix-datum.

När det gäller tidzoner finns många alternativ att välja mellan i Go. Vi kan ange en specifik tidszon för en `time.Time`-typ genom att använda `time.LoadLocation()` och sedan använda `In()`-funktionen för att konvertera tiden till den aktuella tidszonen.

## Se även

- [Go Time Package](https://golang.org/pkg/time/)
- [Go Playground](https://play.golang.org/p/UV7ocap8INl)
- [Gammal artikel om datumkonvertering i Go](https://eli.thegreenplace.net/2009/01/26/dealing-with-time-zones-in-python/)