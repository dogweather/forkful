---
title:                "Omvandling av ett datum till en sträng"
html_title:           "Haskell: Omvandling av ett datum till en sträng"
simple_title:         "Omvandling av ett datum till en sträng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en datum-till-sträng kan vara användbart när du vill visa datuminformation för användare, spara datumet i en databas eller manipulera datumet på olika sätt. Med Haskell kan du enkelt konvertera datum till en sträng med hjälp av några inbyggda funktioner.

## Hur man gör

Först måste du importera modulen "Data.Time" för att kunna använda funktionerna för datum och tid i Haskell. Se till att du också har "Control.Monad" modulen importerad för att använda "liftM" funktionen.

```Haskell
import Data.Time (formatTime, getCurrentTime, defaultTimeLocale)
import Control.Monad (liftM)
```

För att konvertera ett datum till en sträng behöver du två saker: datumet självt och ett formateringssträng. Formateringssträngen berättar för Haskell vilken typ av format du vill ha för ditt datum. Du kan använda olika symboler för år, månad, dag, timme, minut och sekund. Här är ett exempel på hur du kan konvertera dagens datum till en sträng i formatet "ÅÅÅÅ-MM-DD":

```Haskell
getCurrentTime >>= liftM (formatTime defaultTimeLocale "%Y-%m-%d")
```

Du kan också ta en titt på System.Locale modulen för att se alla möjliga symboler du kan använda i formateringssträngen.

## Djupdykning

När du använder formatTime funktionen, är det viktigt att förstå hur tidszoner fungerar. Som standard kommer funktionen att använda den lokala tidszonen på din dator. Om du vill använda en annan tidszon, måste du använda "z" eller "Z" symbolen i din formateringssträng och specificera en tidszon som en parameter. Du kan också använda "X" för att få tidszonen i timmar och minuter.

Haskell erbjuder också en "parseTimeM" funktion som gör det möjligt att konvertera en sträng till ett datum, vilket kan vara användbart om du behöver hämta datumet från en databas eller en användare.

## Se även

- [Haskell Datum och Tid](https://wiki.haskell.org/Date_and_time)
- [Dokumentation för Data.Time-modulen](https://hackage.haskell.org/package/time-1.8.0.2/docs/Data-Time.html)
- [Tutorial för att arbeta med Datum och Tid i Haskell](https://www.schoolofhaskell.com/user/edwardk/tutorial-for-1-0-1-1-time-module)