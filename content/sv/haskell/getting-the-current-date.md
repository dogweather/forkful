---
title:    "Haskell: Att få den aktuella datumet"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför
Att kunna få den aktuella datumet är ett viktigt verktyg för alla Haskell-programmerare. Genom att ha tillgång till datumet kan vi skapa mer dynamiska och relevanta program.

## Hur man gör det
För att få den aktuella datumet i Haskell, kan vi använda funktionen `getCurrentTime` från biblioteket `Data.Time`. Detta bibliotek innehåller många användbara funktioner för hantering av tid och datum i Haskell. För att använda detta bibliotek måste du först importera det i din kod:

```Haskell
import Data.Time
```

När du har importerat biblioteket kan du använda `getCurrentTime` för att få den aktuella tiden som ett `UTCTime`-objekt:

```Haskell
currentDate <- getCurrentTime
```

Detta objekt innehåller information om den aktuella tiden, inklusive datum. För att få tillgång till det datumet kan vi använda funktionen `toGregorian` som omvandlar `UTCTime`-objektet till en trippel med år, månad och dag:

```Haskell
let (year, month, day) = toGregorian $ utctDay currentDate
```

Nu har vi tillgång till år, månad och dag som separata variabler och kan använda dem i vårt program på olika sätt.

## Djupdykning
För de som är intresserade av en mer detaljerad förklaring, så är `getCurrentTime`-funktionen en del av standardbiblioteket `base` och använder sig av systemets klocka för att få den aktuella tiden. Detta innebär att den aktuella tiden kommer att vara baserad på systemets inställningar.

För att få den aktuella tiden i ett specifikt tidszon kan vi använda funktionen `getCurrentTimeZone` från biblioteket `Data.Time.LocalTime` och sedan använda den tillsammans med `utcToLocalTime` för att konvertera tiden till den önskade tidszonen.

```Haskell
import Data.Time.LocalTime

currentTimeZone <- getCurrentTimeZone
let currentDateInTimeZone = utcToLocalTime currentTimeZone currentDate
```

Det finns också möjlighet att få den aktuella tiden i millisekunder genom att använda funktionen `getCurrentTime` från biblioteket `Data.Time.Clock.System`.

## Se även
- [Haskell.org - Datum och tid](https://wiki.haskell.org/Date_and_time)
- [Hackage - Data.Time biblioteket](https://hackage.haskell.org/package/time)
- [Hackage - Data.Time.LocalTime biblioteket](https://hackage.haskell.org/package/time-1.9.2/docs/Data-Time-LocalTime.html)