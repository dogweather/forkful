---
title:                "Haskell: Att få den nuvarande datumet"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna få den nuvarande datumen i dina Haskell-program kan hjälpa dig att hålla koll på tidsrelaterade händelser och också göra det möjligt att göra beräkningar med hjälp av datuminformation. Det kan också vara användbart när du behöver visa eller spara datumen i ditt program.

## Så här gör du

För att få den nuvarande datumen i Haskell kan du använda funktionen `getCurrentTime` från `Data.Time` biblioteket. Först behöver du importera biblioteket i ditt program med följande kod:

```Haskell
import Data.Time
```

Sedan kan du använda `getCurrentTime` funktionen för att få den nuvarande datumen i formatet `UTCTime`. Här är ett exempel som skriver ut den nuvarande datumen på skärmen:

```Haskell
main = do
    currentTime <- getCurrentTime
    print currentTime
```

Output:

```
2021-05-23 10:00:00.123456 UTC 
```

Funktionen `getCurrentTime` returnerar datumen som en typ av `UTCTime` som inte är en läsbar eller användbar form. Du kan dock konvertera den till en mer användbar form som `Day` eller `LocalTime` med hjälp av funktionerna `utctDay` eller `utctToLocalTime` från `Data.Time.LocalTime` biblioteket.

## Djupdykning

Den `UTCTime` som returneras av `getCurrentTime` funktionen representerar det aktuella datumen i UTC-tid, vilket är en standardform av tid som används för att hålla reda på tiden över hela världen. Det är viktigt att känna till att `UTCTime` inte innehåller någon information om tidszonen.

Om du vill få datumen i lokal tid eller i en specifik tidszon kan du använda funktionen `getCurrentTimeZone` från `Data.Time.LocalTime` biblioteket för att få den aktuella tidszonen och sedan konvertera den `UTCTime` till `LocalTime` med hjälp av funktionen `utcToLocalTime` och `getCurrentTimeZone`.

## Se även

- [Haskell documentation for Data.Time library](https://www.haskell.org/cabal/users-guide/developing-packages.html#specifying-dependencies)
- [Stack Overflow: How to get current time in Haskell](https://stackoverflow.com/questions/4297610/how-to-get-current-time-and-date-in-haskell)
- [Haskell wiki: Time library tutorial](https://wiki.haskell.org/Time_library)