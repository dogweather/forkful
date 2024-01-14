---
title:    "Haskell: Att få den nuvarande datumen"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Varför

Att få den nuvarande datumet är en viktig del av många programmeringsprojekt. Det kan användas för att spåra tidslinjer, schemalägga uppgifter eller enkelt för att visa aktuell tid till användare.

## Så här gör du

För att få det nuvarande datumet i Haskell kan du använda funktionen `getCurrentTime` från modulen `Data.Time`. Detta kommer att returnera en instans av typen `UTCTime` som representerar den aktuella tiden och datumet. Du kan sedan bearbeta detta datum som du vill, till exempel konvertera det till en annan tidszon eller extrahera specifika delar som år, månad eller dag.

Här är ett enkelt exempel på hur du kan använda funktionen `getCurrentTime` för att få dagens datum:

```Haskell
import Data.Time

main = do
    now <- getCurrentTime
    let (Just day) = toGregorian . utctDay $ now
    putStrLn $ "Idag är det den " ++ show day ++ "e dagen i månaden."
```

Output:

```
Idag är det den 25e dagen i månaden.
```

## Djupdykning

För att förstå hur funktionen `getCurrentTime` fungerar, låt oss titta på den närmare. I själva verket är det en del av en större familj av funktioner som tillhandahåller tidsrelaterade funktioner i Haskell. Dessa funktioner är del av modulen `Data.Time` som är en del av standardbiblioteket.

Inuti `Data.Time` finns flera moduler som erbjuder olika typer av tidsåtgångar, såsom `Data.Time.Clock` för hantering av klockor och `Data.Time.Calendar` för hantering av datum och månadsvisning. Varje modul erbjuder funktioner som kan hjälpa dig att få och manipulera tider och datum i din applikation.

I fallet med `getCurrentTime`, använder den en kombination av dessa moduler för att få och formatera nuvarande datumen i rätt format.

## Se även

- Här är dokumentationen för `Data.Time` modulen. [Länk](https://hackage.haskell.org/package/time-1.10.0.3/docs/Data-Time.html)
- Läs mer om andra tidsrelaterade funktioner i Haskell i dessa artiklar. [Länk](https://www.tutorialspoint.com/haskell/haskell_date_time.htm)
- För en djupare förståelse av datum och tidshantering i Haskell, kolla in denna video. [Länk](https://www.youtube.com/watch?v=DbVEpHieVtE)