---
title:                "Jämföra två datum"
html_title:           "Haskell: Jämföra två datum"
simple_title:         "Jämföra två datum"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför:
Att jämföra två datum är en vanlig uppgift för programmerare. Det innebär att man kontrollerar vilket av de två datumen som är senare eller tidigare.

Varför gör vi det? I många fall behöver vi veta vilket datum som är senare för att kunna sortera datum i rätt ordning eller för att beräkna antalet dagar mellan två datum.

## Hur man gör:
```Haskell
import Data.Time.Calendar (toGregorian, Day)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)

-- Comparing two dates
compareDates :: Day -> Day -> Ordering
compareDates d1 d2 =
    let (y1, m1, _) = toGregorian d1
        (y2, m2, _) = toGregorian d2
    in compare (y1, m1) (y2, m2)

-- Getting the number of days between two dates
diffDays :: Day -> Day -> Integer
diffDays d1 d2 =
    let (y1, _, _) = toGregorian d1
        (y2, _, _) = toGregorian d2
        (d1', _) = fromOrdinalDate y1
        (d2', _) = fromOrdinalDate y2
    in abs (d1' - d2')
```

Koderna ovan visar hur man kan jämföra två datum och beräkna antalet dagar mellan dem. Vi använder funktionerna `toGregorian` och `fromOrdinalDate` från modulerna `Data.Time.Calendar` och `Data.Time.Calendar.OrdinalDate` för att få ut datumets komponenter (år, månad, dag) och för att konvertera om ett datum till ordinalform.

## Djupdykning:
Att jämföra datum är en vanlig uppgift, speciellt inom områden som finans och logistik där exakta datumen är avgörande. Innan de moderna dataprogrammen, var det vanligt att jämföra datum på papper eller med hjälp av en linjal.

Det finns också alternativa sätt att jämföra datum i Haskell, såsom att använda olika datumtyper eller använda funktioner från andra moduler som `Data.Time.Clock` för att hantera tidszon och tid.

## Se även:
- [Haskell documentation for Data.Time.Calendar](https://hackage.haskell.org/package/time/docs/Data-Time-Calendar.html)
- [Haskell documentation for Data.Time.Calendar.OrdinalDate](https://hackage.haskell.org/package/time/docs/Data-Time-Calendar-OrdinalDate.html)
- [Haskell documentation for Data.Time.Clock](https://hackage.haskell.org/package/time/docs/Data-Time-Clock.html)