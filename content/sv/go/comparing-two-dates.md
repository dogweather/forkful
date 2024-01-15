---
title:                "Jämförande av två datum"
html_title:           "Go: Jämförande av två datum"
simple_title:         "Jämförande av två datum"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum kan vara en viktig del av många program, särskilt när det gäller att sortera och filtrera data. I Go kan du enkelt jämföra två datum med hjälp av inbyggda funktioner, vilket sparar tid och minimerar risken för fel.

## Hur man gör

För att jämföra två datum i Go behöver du först importera paketet "time". Sedan kan du använda följande funktioner för att jämföra två datum:

- **Equal**: jämför om två datum är exakt lika.
- **Before**: jämför om ett datum kommer före ett annat.
- **After**: jämför om ett datum kommer efter ett annat.

Här är ett exempel på hur du kan använda dessa funktioner:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    datum1 := time.Date(2020, time.November, 1, 0, 0, 0, 0, time.UTC)
    datum2 := time.Date(2019, time.December, 1, 0, 0, 0, 0, time.UTC)

    fmt.Println(datum1.Equal(datum2)) // false
    fmt.Println(datum1.Before(datum2)) // false
    fmt.Println(datum1.After(datum2)) // true
}
```

I detta exempel skapar vi två olika datum och jämför dem sedan med hjälp av de olika funktionerna. Observera att vi använder paketet "fmt" för att skriva ut resultaten.

## Djupdykning

När du jämför två datum i Go bör du tänka på att både datumet och tiden måste vara exakt lika för att funktionerna ska returnera "true". Om en av dem har ett annat värde kommer funktionen att returnera "false".

Go erbjuder också andra funktioner för att arbeta med datum, till exempel **AddDate** för att lägga till dagar, månader eller år till ett datum och **Sub** för att subtrahera tid från ett datum.

Det finns också andra metoder för att utföra mer avancerade jämförelser, till exempel att kontrollera om två datum är inom samma tidsintervall eller om de är samma dag eller samma veckodag.

## Se även

- Go's tidpaketets dokumentation: https://golang.org/pkg/time/
- Översikt över Go-programmeringsspråket: https://golang.org/doc/