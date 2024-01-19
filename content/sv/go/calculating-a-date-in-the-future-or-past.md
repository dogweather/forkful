---
title:                "Beräkna ett datum i framtiden eller förflutna"
html_title:           "Go: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

---
## Vad och varför?
Att beräkna framtidens eller förflutnas datumen innebär att vi lägger till eller tar bort en specifik tid från ett befintligt datum. Programmerare gör det för att hantera schemaläggningar, tidsbegränsningar m.m.

## Hur man gör:
Med Go kan du lätt beräkna ett framtida eller förgånget datum. Använda `AddDate(years int, months int, days int)` funktionen.

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    date := time.Now()
    newDate := date.AddDate(1, 2, 3)

    fmt.Println("Ursprungligt datum:", date.Format("2006-01-02"))
    fmt.Println("Nytt datum:", newDate.Format("2006-01-02"))
}
```
I ovanstående kod lägger vi till 1 år, 2 månader och 3 dagar till det aktuella datumet.

## Djupdykning
På de tidiga dagarna av datorprogrammering var tidsberäkning svårt och buggigt. Tidszoner, skottår och andra komplikationer gjorde detta till en utmaning. Med moderna språk som Go har dessa problem tagits om hand.

Alternativ till `AddDate` inkluderar att direkt förändra datumet med den standard `+` operatorn, eller att använda tredje partens bibliotek för mer komplexa datum och tidsoperationer.

I Go, `AddDate` fungerar internt genom att skapa ett nytt `Date` objekt baserat på det existerande, sedan lägga till eller dra bort de specificerade åren, månaderna och dagarna.

## Se även
- [Go Dokumentation om datum och tid](https://golang.org/pkg/time/)
- [Tutorial om datum och tid i Go](https://www.programming-books.io/essential/go/date-and-time-96df1138f8d24856b5c99062bfa8bd35)
- [Go Time-paketet](https://gobyexample.com/time)
---