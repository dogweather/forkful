---
title:                "Beräkna ett datum i framtiden eller förflutna"
html_title:           "Haskell: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna räkna ut ett datum i framtiden eller det förflutna är användbart i många situationer, som att planera evenemang eller hålla koll på födelsedagar. Med hjälp av Haskell kan du enkelt skapa kod som kan utföra dessa beräkningar åt dig.

## Så här gör du

Det första steget för att kunna räkna ut ett datum i framtiden eller det förflutna är att förstå hur datum representeras i Haskell. I Haskell finns det en inbyggd typ för datum som heter `Day`, som representerar ett datum med år, månad och dag. Du kan skapa ett `Day`-värde genom att använda funktionen `fromGregorian`, som tar år, månad och dag som argument.

```Haskell
import Data.Time.Calendar

day = fromGregorian 2021 10 31
```

För att räkna ut ett datum i framtiden eller det förflutna behöver vi också använda oss av funktionen `addDays`, som tar ett `Day`-värde och ett antal dagar som argument och returnerar ett nytt `Day`-värde. Antalet dagar kan vara både positivt och negativt, beroende på om du vill räkna framåt eller bakåt.

```Haskell
tomorrow = addDays 1 day
yesterday = addDays (-1) day
```

Det går också att kombinera flera `addDays`-funktioner för att till exempel räkna ut ett datum tre dagar framåt.

```Haskell
threeDaysAhead = addDays 1 (addDays 1 (addDays 1 day))
```

För att kunna använda `Day`-värden i dina program måste du också importera modulen `Data.Time.Calendar` med ett `import`-uttryck.

## Djupdykning

I Haskell finns det också en typ för tider, `TimeOfDay`, som representerar en specifik tid på dagen med timmar, minuter och sekunder. Med hjälp av funktionen `fromGregorian`, som vi använde tidigare, tillsammans med funktionen `timeOfDayToTime` kan vi kombinera ett `Day`-värde med en `TimeOfDay` för att få ett komplett datum och tid. Detta kan vara användbart om du till exempel behöver räkna ut ett specifikt tidpunkt i framtiden eller det förflutna.

```Haskell
import Data.Time.Calendar
import Data.Time.LocalTime

day = fromGregorian 2021 10 31
time = timeOfDayToTime (TimeOfDay 18 30 0)
dateTime = LocalTime day time -- dateTime är ett komplett datum och tid
```

## Se även

För mer information om `Day`-typen och andra funktioner för datum och tid i Haskell, se följande länkar:

- Dokumentation för `Data.Time.Calendar`: https://hackage.haskell.org/package/time/docs/Data-Time-Calendar.html
- Dokumentation för `Data.Time.LocalTime`: https://hackage.haskell.org/package/time/docs/Data-Time-LocalTime.html