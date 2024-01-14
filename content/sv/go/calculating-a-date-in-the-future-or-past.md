---
title:                "Go: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att beräkna ett datum i framtiden eller det förflutna kan vara användbart för att till exempel planera evenemang eller följa deadline för projekt. Med hjälp av Go programming kan du enkelt skriva kod som gör detta åt dig.

## Så här gör du

Först behöver du importera paketet "time" som innehåller funktioner för att hantera tid och datum. Sedan kan du använda funktionen "AddDate(years, months, days)" för att lägga till eller dra bort år, månader och dagar från ett specifikt datum.

Här är ett enkelt exempel på hur du kan räkna ut ett datum i framtiden:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    t := time.Now() //Hämtar aktuell tid
    futureDate := t.AddDate(1, 0, 0) //Lägger till 1 år till aktuell tid
    fmt.Println("Datumet om ett år kommer vara:", futureDate)
}
```

Körning av denna kod kommer att ge dig följande output: "Datumet om ett år kommer vara: 2022-07-22 15:59:14.220266979 +0200 CEST m=+365.000203684"

Du kan också beräkna ett datum i det förflutna genom att använda negativa värden i "AddDate()". Här är ett exempel där vi räknar ut datumet för ett halvt år sedan:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    t := time.Now() //Hämtar aktuell tid
    pastDate := t.AddDate(0, -6, 0) //Dra av 6 månader från aktuell tid
    fmt.Println("Datumet för ett halvt år sedan var:", pastDate)
}
```

Outputen kommer att bli "Datumet för ett halvt år sedan var: 2020-01-22 15:59:14.332787894 +0100 CET m=-180.000097499"

## Djupdykning

För att räkna ut ett datum i framtiden eller förflutna tar "AddDate()" funktionen hänsyn till antal dagar i varje månad och skottår. Till exempel, om du lägger till 1 månad till 31 januari kommer det resulterande datumet att vara 28 februari om det inte är ett skottår.

Man kan också använda funktionen "Date(year, month, day, hour, min, sec, nsec, loc)" för att skapa ett specifikt datum och sedan beräkna ett datum i förflutna eller framtiden utifrån det.

## Se också

* https://golang.org/pkg/time/#AddDate
* https://golang.org/pkg/time/#Date