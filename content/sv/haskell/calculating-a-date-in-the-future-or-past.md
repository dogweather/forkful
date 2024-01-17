---
title:                "Beräkning av ett datum i framtiden eller det förflutna"
html_title:           "Haskell: Beräkning av ett datum i framtiden eller det förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller det förflutna"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Räkna ut ett datum i framtiden eller det förflutna är en vanlig uppgift inom programmering. Ofta behöver vi veta när en viss händelse kommer att ske eller när en tidsfrist löper ut. Genom att kunna beräkna detta kan vi underlätta vår planering och förutse eventuella problem.

## Så här:

För att beräkna ett datum i framtiden eller förflutna i Haskell används funktionen `addDays` från biblioteket `Data.Time.Calendar`. Funktionen tar två argument, det första är antalet dagar som ska adderas eller subtraheras och det andra är det ursprungliga datumet.

```Haskell
import Data.Time.Calendar

-- Lägger till 10 dagar till dagens datum
addDays 10 today

-- Subtraherar 5 dagar från ett specifikt datum
addDays (-5) (fromGregorian 2021 10 15)
```

## Djupdykning:

Beräkning av datum är en viktig del av många programmeringsspråk, och Haskell är inget undantag. Det finns dock olika alternativ för hur man kan göra detta, bland annat använda sig av olika bibliotek eller skriva egna funktioner. Vissa programmeringsspråk har inbyggda metoder för datumberäkningar, medan det i Haskell krävs att man importerar ett bibliotek för detta ändamål.

En annan viktig aspekt är att olika tidszoner och datumformat kan påverka beräkningen. Det är därför viktigt att alltid specificera vilken tidszon och vilket format man vill använda sig av vid beräkningen.

Det kan även vara bra att känna till att mätningen av tid i datorer är baserad på Unix-epoken, vilket är 1 januari 1970. Detta kan påverka beräkningen om man använder sig av äldre datum före denna tidpunkt.

## Se även:

- [Data.Time.Calendar dokumentation](https://hackage.haskell.org/package/time-1.11.1/docs/Data-Time-Calendar.html)
- [Beräkning av datum i JavaScript](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/Date/setDate)
- [Beräkning av datum i Python](https://docs.python.org/3/library/datetime.html#datetime.datetime)