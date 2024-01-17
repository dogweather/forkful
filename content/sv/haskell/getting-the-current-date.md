---
title:                "Att få den aktuella datumet"
html_title:           "Haskell: Att få den aktuella datumet"
simple_title:         "Att få den aktuella datumet"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att få den nuvarande datumet kan vara användbart för programmörer på många sätt. Det hjälper till med att skapa timestamp, organisera filer baserat på datum, och även för att skapa datumstämplar för loggar eller datainsamling. Det är i princip ett sätt att få en exakt tidpunkt för ett visst ögonblick.

## Hur man:
För att få den nuvarande datumet i Haskell, kan du använda funktionen 'getCurrentTime' från modulen Data.Time.Clock. Detta ger tillbaka en IO-datetyp, som måste exekversa i en monad. Här är ett enkelt exempel på hur man kan skriva ut resultatet:

```Haskell
import Data.Time.Clock (getCurrentTime)
main = do
    t <- getCurrentTime
    print t
```

Output från detta kommer att se ut ungefär såhär:
```Haskell
2019-10-08 16:35:10.607701953 UTC
```

Du kan också få den nuvarande datumet i annorlunda format, till exempel som en sträng istället för en IO-datetyp. Detta kan åstadkommas med hjälp av funktionen 'formatTime' från modulen Data.Time.Format. Här är ett exempel på hur du kan få datumet i formatet "DD-MM-YYYY":

```Haskell
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
main = do
    t <- getCurrentTime
    let formattedTime = formatTime defaultTimeLocale "%d-%m-%Y" t
    print formattedTime
```

Output från detta kommer att se ut som följande:
```Haskell
08-10-2019
```

## Deep Dive:
Att få den nuvarande datumet i Haskell började som ett problem 2003 då ingen inbyggd funktion fanns tillgänglig. Detta ledde till utvecklingen av modulen Data.Time. Dock finns det också alternativ såsom modulen System.Time och Data.Time.Clock.POSIX, som ger liknande funktioner. Det finns också en skillnad i precision mellan dessa olika moduler, där Data.Time ger mer noggrannhet jämfört med System.Time.

## Se även:
- [Haskell Dokumentation för getCurrentTime](https://www.haskell.org/hoogle/?hoogle=getCurrentTime)
- [Haskell Dokumentation för Data.Time](https://www.haskell.org/hoogle/?hoogle=Data.Time)
- [Haskell Dokumentation för System.Time](https://www.haskell.org/hoogle/?hoogle=System.Time)
- [Haskell Dokumentation för Data.Time.Clock.POSIX](https://www.haskell.org/hoogle/?hoogle=Data.Time.Clock.POSIX)