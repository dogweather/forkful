---
title:                "Go: Hämta aktuellt datum"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Varför

Att känna till det aktuella datumet är en viktig del av programmering, eftersom det ofta är en avgörande faktor för genomförandet av olika åtgärder. Det är också avgörande för att se till att applikationen fungerar korrekt och inte orsakar problem för användarna.

## Så här gör du

För att få det aktuella datumet i Go, kan vi använda funktionen `Now()` från paketet `time`. Vi kan också använda `Format()` funktionen för att formatera datumet enligt våra önskemål. Här är en kodexempel som visar hur man kan få det aktuella datumet och formatera det till en vanlig datumsträng.

```Go
import "fmt"
import "time"

func main() {
    today := time.Now()
    formattedDate := today.Format("02-01-2006")
    fmt.Println(formattedDate)
}
```

När vi kör detta program kommer det att skriva ut det aktuella datumet i formatet "DD-MM-YYYY", till exempel "19-08-2020". Du kan experimentera med olika formateringssträngar för att få det datumformat du behöver.

## Djupdykning

Nu när vi har fått det grundläggande datumet i ett läsbart format, låt oss titta på några andra användbara funktioner från paketet `time`.

### Tidszoner

Vi kan också få det aktuella datumet och klockslaget för en specifik tidszon. Det kan vara användbart om applikationen ska användas av människor över hela världen. Till exempel, om vi vill ha det nuvarande datumet och klockslaget för London, kan vi använda följande kod:

```Go
londonTime := time.Now().In(time.FixedZone("GMT+1", 3600))
fmt.Println(londonTime)
```

Detta kommer att skriva ut det nuvarande datumet och klockslaget för London, justerat för tidszonen "GMT+1".

### Datummatematik

Vi kan också använda `time` paketet för att utföra datummatematik, som att lägga till eller subtrahera tid från ett befintligt datum. Till exempel, om vi vill ha det datum som är 7 dagar från idag, kan vi göra så här:

```Go
futureDate := time.Now().AddDate(0, 0, 7)
fmt.Println(futureDate)
```

Detta kommer att skriva ut datumet som är 7 dagar från nu.

## Se även

- [Go `time` paketet dokumentation](https://golang.org/pkg/time/)
- [En komplett guide till datum- och tidsberäkningar i Go](https://yourbasic.org/golang/date-time--format-parse-string-output/)
- [Hämta aktuellt datum och tid i Go](https://www.calhoun.io/how-to-get-the-current-date-time-in-go/)