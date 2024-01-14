---
title:                "Go: Jämförande av två datum"
simple_title:         "Jämförande av två datum"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum är en vanlig uppgift inom programmering, särskilt när man arbetar med tid och datum i sina projekt. Genom att jämföra två datum kan man till exempel se om en händelse har inträffat före eller efter ett visst datum, eller om det har gått en viss tid sedan ett specifikt datum. I denna bloggpost kommer vi att gå igenom hur man kan jämföra två datum i Go-programmeringsspråket.

## Hur man gör

Det första vi behöver göra är att importera Go's tidspaket, som innehåller funktioner för att hantera tid och datum. Sedan kan vi använda funktionen `After` för att jämföra två datum. Låt oss säga att vi har två variabler som innehåller datum i Go's tidslayout:

```Go
import "time"

datum1 := "2020-01-01"
datum2 := "2020-02-01"

t1, _ := time.Parse("2006-01-02", datum1)
t2, _ := time.Parse("2006-01-02", datum2)
```

I detta fall kan vi använda `After`-funktionen för att se om `datum2` inträffade efter `datum1`:

```Go
if t2.After(t1) {
    fmt.Printf("%s är senare än %s", datum2, datum1)
}
```

Vi kan också använda `Before`-funktionen för att se om ett datum inträffade före ett annat. Dessutom kan vi använda `Equal`-funktionen för att kontrollera om två datum är samma.

## Djupdykning

När vi arbetar med tid och datum måste vi också tänka på tidszoner och hur de påverkar jämförelser av datum. I Go's tidspaket finns det en funktion som heter `Location` som gör det möjligt för oss att ändra tidszonen på ett datum. Till exempel, om vi vill jämföra två datum i olika tidszoner, kan vi använda följande kod:

```Go
datum1 := "2020-01-01 15:00:00"
datum2 := "2020-01-02 03:00:00"

loc1, _ := time.LoadLocation("Europe/Stockholm")
loc2, _ := time.LoadLocation("Australia/Sydney")

t1, _ := time.ParseInLocation("2006-01-02 15:04:05", datum1, loc1)
t2, _ := time.ParseInLocation("2006-01-02 15:04:05", datum2, loc2)

if t2.After(t1) {
    fmt.Printf("%s är senare än %s", datum2, datum1)
}
```

Som vi kan se i detta exempel, använder vi `LoadLocation`-funktionen för att ladda in de olika tidszonerna och sedan `ParseInLocation`-funktionen för att konvertera datumen i respektive tidszon innan vi jämför dem.

## Se även

Här är några länkar till Go's dokumentation som kan vara till hjälp när du jämför datum:

- [Go Tidspaket dokumentation](https://golang.org/pkg/time)
- [Go Tidslayouter](https://yourbasic.org/golang/format-parse-string-time-date-example)
- [Go Tidszoner](https://golang.org/pkg/time/#LoadLocation)