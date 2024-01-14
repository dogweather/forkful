---
title:                "Go: Konvertering av datum till sträng"
simple_title:         "Konvertering av datum till sträng"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför
I denna artikel kommer vi att utforska ett viktigt koncept inom Go-programmering: konvertering av datum till strängar. Genom att lära oss hur man gör detta kan vi skriva kod som bättre hanterar datum och tidsåtgångar.

## Hur man gör
Konvertering av datum till strängar är en relativt enkel process i Go. Det finns två vanliga metoder som kan användas, beroende på dina behov.

En metod är att använda "Format" -funktionen från paketet "time". Denna funktion tar ett datum- eller tidsobjekt och en formatsträng som argument och returnerar en sträng baserad på formatet som specifierats.

```Go
import (
    "fmt"
    "time"
)

func main() {
    t := time.Now()
    fmt.Println(t.Format("Jan 2, 2006"))
}
```

Denna kod skulle ge följande utdata: "Nov 15, 2021". Datumformatet som används här är "Jan 2, 2006", vilket är en konvention som är specifik för Go.

En annan metod är att använda "strconv" -paketet och dess "Itoa" -funktion. Denna funktion konverterar ett heltal till en sträng. Genom att först konvertera ett datumobjekt till en Unix-timestamp och sedan till ett heltal, kan vi sedan använda "Itoa" för att konvertera det till en sträng.

```Go
import (
    "fmt"
    "strconv"
    "time"
)

func main() {
    t := time.Now()
    unix := t.Unix()
    fmt.Println(strconv.Itoa(int(unix)))
}
```

Denna kod skulle ge utdatan: "1636978109". Det är viktigt att notera att denna metod inte ger en formaterad sträng som "Format" -metoden gör, men den ger en unik identifierare för ett datum.

## Djupdykning
Vid konvertering av datum till strängar är det viktigt att förstå datumformatet som används för att få önskad output. Som nämnts tidigare följer Go en konvention för datumformat som använder ord som "Jan" för månad, "2" för dag och "2006" för år. Dessa kan ändras beroende på önskad output.

Dessutom är det viktigt att tänka på tidszonen när man hanterar datum och tider. Go har inbyggda funktioner för att hantera tidszoner och konvertera mellan olika zoner.

## Se också
- [Go Tidspaket](https://golang.org/pkg/time/)
- [strconv-paketet](https://golang.org/pkg/strconv/)