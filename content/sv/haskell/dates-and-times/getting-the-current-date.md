---
title:                "Få det aktuella datumet"
date:                  2024-02-03T19:09:39.184184-07:00
model:                 gpt-4-0125-preview
simple_title:         "Få det aktuella datumet"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att hämta det aktuella datumet i Haskell innebär att få systemets nuvarande tid och omvandla den till ett läsbart datumformat. Programmerare gör detta för att utföra operationer baserade på datumet, såsom loggning, schemaläggning av uppgifter eller tidstämpling av händelser i applikationer.

## Hur man gör:
Haskells standardbibliotek, `base`, tillhandahåller `Data.Time`-modulen som erbjuder funktionalitet för att arbeta med datum och tider. Så här använder du den för att få det aktuella datumet:

```haskell
import Data.Time (getCurrentTime, utctDay)

main :: IO ()
main = do
    now <- getCurrentTime
    let today = utctDay now
    print today
```

Exempelutdata:
```
2023-04-12
```

För mer flexibilitet, som att formatera datumet eller arbeta med olika tidszoner, är `time`-biblioteket ovärderligt. Så här kan du formatera det aktuella datumet:

```haskell
import Data.Time

main :: IO ()
main = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
    putStrLn $ formatTime defaultTimeLocale "%Y-%m-%d" zoneNow
```

Detta skriver ut det aktuella datumet i formatet `ÅÅÅÅ-MM-DD`, justerat till den lokala tidszonen.

Dessutom, för stöd från tredjepartsbibliotek, rekommenderas `time` starkt och används ofta inom Haskell-gemenskapen för dess omfattande möjligheter till datum- och tidshantering. Exemplen ovan använder detta bibliotek.

Om du behöver mer omfattande datumhantering, inklusive tolkning från strängar eller aritmetiska operationer med datum och tider, kommer utforskning av ytterligare funktioner inom `Data.Time` att vara fördelaktigt.
