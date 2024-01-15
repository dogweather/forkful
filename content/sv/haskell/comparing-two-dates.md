---
title:                "Jämföring av två datum"
html_title:           "Haskell: Jämföring av två datum"
simple_title:         "Jämföring av två datum"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför 

Att jämföra två datum kan vara en viktig del av många program, från att kontrollera användares ålder till att sortera data efter datum. Genom att använda Haskell, kan man enkelt utföra denna jämförelse och hantera olika datumformat.

## Hur man gör

Först behöver vi importera `Data.Time` modulen för att hantera datum och tid i Haskell. Sedan definierar vi två datum i önskat format:

```Haskell 
import Data.Time

start <- parseTimeM True defaultTimeLocale "%Y-%m-%d" "2021-01-01"
end <- parseTimeM True defaultTimeLocale "%Y-%m-%d" "2021-02-01"
```

Sedan kan vi använda de inbyggda funktionerna `compare` och `diffDays` för att jämföra och beräkna antalet dagar mellan de två datumen:

```Haskell 
compare start end
-- LT (mindre än)

diffDays end start
-- 31 (antalet dagar mellan datumet)
```

Vi kan också använda `diffUTCTime` för att jämföra tider, och `addDays` för att lägga till ett visst antal dagar till ett datum.

## Djupdykning

Haskell har en stark typ-system som kan vara till hjälp när man arbetar med datum och tider. Genom att använda `Data.Time.Calendar.Days` modulen, kan man definiera datatyperna `Day` och `DiffDays` för att representera datum och antal dagar.

En annan användbar funktion är `formatTime` som kan användas för att konvertera ett datum till olika format. Det finns också flera andra moduler för att hantera tidszon, som `Data.Time.LocalTime` och `Data.Time.TimeZone`.

Att förstå och använda dessa moduler kan hjälpa till att undvika vanliga problem som att hantera sommar- och vintertid och tidszoners skillnader.

## Se även

- [Haskell dokumentation för Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Handbok för datum och tider i Haskell](https://www.cs.yale.edu/homes/hudak/Papers/HSoC/HSoC.html)