---
title:    "Swift: Jämföra två datum"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Varför
Att jämföra två datum kan vara användbart i många olika situationer. Det kan till exempel hjälpa till att avgöra om en händelse har inträffat före eller efter ett visst datum, eller om det skiljer en viss tidsperiod mellan två händelser.

## Så här gör du
För att jämföra två datum i Swift finns det flera metoder att använda. En vanlig metod är att använda `timeIntervalSinceDate()` som returnerar antalet sekunder mellan två datum. Detta kan sedan jämföras med `TimeInterval` för att få reda på om det skiljer en viss tidsperiod mellan de två datumen.

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"
let date1 = dateFormatter.date(from: "2021-01-01")
let date2 = dateFormatter.date(from: "2021-02-01")

let timeDifference = date2.timeIntervalSinceDate(date1)
// timeDifference = 2678400 seconds
```

En annan metod är att använda `compare()` som jämför två datum och returnerar en `ComparisonResult` som antingen är `.orderedAscending` om det första datumet är tidigare än det andra, `.orderedDescending` om det första datumet är senare än det andra, eller `.orderedSame` om de är samma.

```Swift
let compareResult = date1.compare(date2)
// compareResult = .orderedAscending
```

## Djupdykning
När man jämför datum är det viktigt att ha rätt format på datumen. Om formatet inte stämmer kan det leda till felaktiga jämförelser. Det är också viktigt att komma ihåg olika tidszoner och att omvandla datumen till en gemensam tidszon innan jämförelsen görs.

Att hantera tidszoner kan vara komplicerat och det kan vara en bra idé att använda sig av en tidszonspecifik formatterare som `DateFormatter` för att korrekt formatera datumen till rätt tidszon.

## Se även
- [Date and Time Programming Guide](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DatesAndTimes/DatesAndTimes.html)
- [Dokumentation för Date-klassen i Swift](https://developer.apple.com/documentation/foundation/date)