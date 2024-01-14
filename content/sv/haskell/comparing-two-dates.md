---
title:    "Haskell: Jämföring av två datum."
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

I Haskell är det möjligt att jämföra två datum med hjälp av olika funktioner. Detta kan vara användbart när man behöver hantera datum i sina program, till exempel för att sortera data eller filtrera det efter ett visst datum. 

## Så här gör du

Att jämföra datum i Haskell är enkelt och kan göras på flera olika sätt, beroende på vilken precision man behöver. Här är ett exempel på hur man kan jämföra två datum och få ut resultatet som en matematisk sanning (True eller False):

```Haskell
import Data.Time
import Data.Time.Calendar.OrdinalDate

date1 = fromGregorian 2021 03 02
date2 = fromGregorian 2021 05 15

-- jämför om date1 är mindre än date2
date1 < date2
-- Output: True
```

Man kan även jämföra datum baserat på deras kalenderdatum och tidszon. Här är ett exempel på hur man kan jämföra två datum och få ut deras exakta skillnad:

```Haskell
-- jämför två datum baserade på deras tidszon
zonedTime1 = ZonedTime (LocalTime date1 (TimeOfDay 12 00 00)) (TimeZone 120 True "CET")
zonedTime2 = ZonedTime (LocalTime date2 (TimeOfDay 20 00 00)) (TimeZone (-60) True "PST")

zonedTime1 < zonedTime2
-- Output: False

-- jämför två datum baserade på deras kalenderdatum
diffDays date1 date2
-- Output: 73
```

Som du kan se i det andra exemplet, kan man även få ut skillnaden mellan två datum i antal dagar genom att använda den inbyggda funktionen `diffDays`. Det finns också andra liknande funktioner som `diffLocalTime` och `diffTimeOfDay` för att få ut exakta skillnader baserat på datumens olika komponenter.

## Djupdykning

Att jämföra datum i Haskell involverar att först och främst omvandla dem till rätt format, och sedan använda de olika funktionerna som finns tillgängliga i språket. Det finns också en hel del andra inbyggda funktioner som kan vara användbara för datumhantering, som till exempel `addDays`, `addUTCTime`, och `formatTime`. Det kan vara värt att utforska dessa närmare när du arbetar med datum i dina program.

## Se även

- [Haskell.org](https://www.haskell.org/)
- [Haskell Wikibook - Datum och tid](https://en.wikibooks.org/wiki/Haskell/Datum_och_tid)
- [Hoogle - sökmotor för Haskell-funktioner](https://hoogle.haskell.org/)