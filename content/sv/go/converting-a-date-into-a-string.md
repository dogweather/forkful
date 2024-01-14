---
title:    "Go: Konvertera ett datum till en sträng"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna konvertera ett datum till en sträng är en vanlig uppgift inom programmering. Det kan användas för att visa datum i ett användarvänligt format eller för att spara datum i en databas. I Go finns det flera inbyggda funktioner som underlättar denna process.

## Så här gör man

För att konvertera ett datum till en sträng i Go, använder vi funktionen `Format` från paketet `time`. Här är ett exempel på hur man kan konvertera dagens datum till en sträng i formatet "dd/mm/yyyy":

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Skapar ett tidsobjekt för dagens datum
    idag := time.Now()

    // Använder funktionen Format för att konvertera tiden till en sträng
    str := idag.Format("02/01/2006")

    // Skriver ut strängen
    fmt.Println(str)
}

// Output:
// 28/10/2021
```

Det finns många olika formatmallar som kan användas för att konvertera datum till strängar, beroende på hur man vill att datumet ska visas. Här är några exempel på formatmallar:

- `"02/01/2006"` för att visa datum i formatet dag/månad/år
- `"01-02-2006"` för att visa datum i formatet månad-dag-år
- `"2006 Jan 02"` för att visa datum i formatet år månad dag (exempelvis 2021 Okt 28)

Man kan även kombinera flera olika formatmallar för att få ett mer specifikt format.

## Djupdykning

Som vi såg i exemplet ovan, behöver vi ange ett specifikt format när vi använder funktionen `Format` för att konvertera datum till strängar. Men vad betyder egentligen de olika siffrorna och bokstäverna i formatmallen?

För att förstå det behöver vi titta på en grundläggande regel inom Go: datumformatet "2" representerar en "padad" siffra och "01" representerar en "padad" tvåsiffrig siffra. Detta innebär att om vår månad exempelvis bara är en siffra (t.ex. "1" för januari), kommer Go automatiskt att lägga till en nolla framför (t.ex. "01").

Här är en tabell som visar betydelsen av varje siffra och bokstav i en formatmall:

| Siffra/bokstav | Betydelse                                      |
| -------------- | ----------------------------------------------- |
| 2              | En padad siffra (t.ex. "02" istället för "2")   |
| 01             | En padad tvåsiffrig siffra (t.ex. "01" istället för "1") |
| 2006           | Året i fyra siffror (t.ex. "2021")              |
| Jan            | Första tre bokstäverna av månadsnamnet (t.ex. "Jan" för januari) |
| 01             | En padad tvåsiffrig siffra för månaden (t.ex. "01" för januari) |
| Mon            | Hela namnet på månaden (t.ex. "January" för januari) |
| 15             | Timmar (vanligtvis "3" eller "15")              |
| 04             | Minuter (vanligtvis "7" eller "04")             |
| 05             | Sekunder (vanligtvis "9" eller "05")            |
| PM             | Visar om det är eftermiddag eller kväll (t.ex. "AM" eller "PM") |

Man kan också använda andra specialtecken i formatmallen för att få till exempelvis en radbrytning eller ett kolon i resultatet. Här är några exempel på sådana specialtecken:

- `"/"` för att få en radbrytning
- `"-"` för att få ett mellanslag
- `":"` för att få ett kolon

För att se en fullständig lista på alla formatmallar