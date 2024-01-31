---
title:                "Att hämta aktuellt datum"
date:                  2024-01-20T15:14:50.476062-07:00
simple_title:         "Att hämta aktuellt datum"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Hämta det aktuella datumet är en vanlig uppgift i kodning. Vi gör det för att spåra händelser, logga data, eller för funktioner som påminnelser och deadlines. 

## Hur man gör:
Här är ett snabbt exempel på hur man får det aktuella datumet i Go:

```go
package main

import (
    "fmt"
    "time"
)

func main() {
    now := time.Now()
    fmt.Println("Det aktuella datumet och tiden är:", now)
}
```

Kör koden, och du får något liknande:

```
Det aktuella datumet och tiden är: 2023-03-28 15:04:05.123456 +0200 CEST
```

## Fördjupning:
Att få det aktuella datumet är inte unikt för Go; nästan alla moderna programmeringsspråk stödjer det. 

Historiskt, har programmerare alltid haft behov av att arbeta med tiden: att spara när en fil skapades eller när en databaspost uppdaterades. I Go hanterar vi datum och tid genom `time` paketet, som tillhandahåller funktionalitet för att få nuvarande data och tid och för datum/tid manipulation.

Alternativen för att hämta datum och tid kan variera beroende på språket men grunderna är ofta desamma. I Go är `time.Now()` metoden att gå till, men i andra språk kan det vara annorlunda, såsom `datetime.now()` i Python eller `new Date()` i JavaScript.

När det gäller implementeringsdetaljer, använder `time.Now()` i Go monotonic klockan om möjligt för att säkerställa att tiden är konsekvent mellan olika klockslag och över flera uppdateringar. Det är också värt att notera att tiden är representerad i UTC, vilket är den universella tidsstandarden.

## Se också:
- Go's officiella dokumentation om `time` paketet: https://pkg.go.dev/time
- En artikel om datum- och tidshantering i Go: https://yourbasic.org/golang/time-change-format/
- En jämförelse av datum och tid i olika programmeringsspråk: https://en.wikipedia.org/wiki/System_time
