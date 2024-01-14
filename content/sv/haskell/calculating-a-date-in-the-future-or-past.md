---
title:    "Haskell: Beräkning av ett datum i framtiden eller förflutna"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna ett datum i framtiden eller i det förflutna kan vara användbart i många olika situationer inom programmering. Det kan till exempel hjälpa dig att planera för framtida händelser eller att hålla koll på när saker och ting inträffade.

## Hur man gör det

För att beräkna ett datum i Haskell kan du använda funktionen `addDays` och `addGregorianYearsRollOver`. Dessa funktioner tillåter dig att lägga till eller subtrahera ett visst antal dagar eller år från ett befintligt datum.

``` Haskell
import Data.Time

-- Lägg till 10 dagar till idag
addDays 10 $ fromGregorian 2021 9 1
-- Output: 2021-09-11

-- Subtrahera 3 år från idag
addGregorianYearsRollOver (-3) $ fromGregorian 2021 9 1
-- Output: 2018-09-01
```

Du kan också använda funktionen `diffDays` för att räkna ut antalet dagar mellan två datum.

``` Haskell
import Data.Time

-- Antal dagar mellan två datum
diffDays (fromGregorian 2021 9 1) (fromGregorian 2021 8 1)
-- Output: 31
```

## Djupdykning

I Haskell är datum och tid representerade som olika datatyper. För att kunna utföra beräkningar med dessa datatyper behöver du använda dig av funktioner som är specifika för varje datatyp. Till exempel, för att räkna ut antalet dagar mellan två datum behöver du använda funktionen `diffDays` som är specifik för `Day` datatypen.

Det är också viktigt att notera att Datum och Tid i Haskell är immutabla datatyper, vilket innebär att de inte går att ändra på. Om du vill ändra ett datum eller en tid måste du i stället skapa ett nytt objekt med dina önskade förändringar.

## Se även

- [Haskell Documentation: Data.Time](https://downloads.haskell.org/~ghc/latest/docs/html/library/time-1.11.1.0/Data-Time.html)
- [Haskell for all: How to deal with timezones in Haskell](https://www.haskellforall.com/2021/03/how-to-deal-with-timezones-in-haskell.html)
- [Learn You a Haskell: Working with Dates and Times](https://learnyouahaskell.com/starting-out#ready-set-go)