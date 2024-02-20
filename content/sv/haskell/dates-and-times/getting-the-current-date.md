---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:39.184184-07:00
description: "Att h\xE4mta det aktuella datumet i Haskell inneb\xE4r att f\xE5 systemets\
  \ nuvarande tid och omvandla den till ett l\xE4sbart datumformat. Programmerare\
  \ g\xF6r detta f\xF6r\u2026"
lastmod: 2024-02-19 22:04:57.186288
model: gpt-4-0125-preview
summary: "Att h\xE4mta det aktuella datumet i Haskell inneb\xE4r att f\xE5 systemets\
  \ nuvarande tid och omvandla den till ett l\xE4sbart datumformat. Programmerare\
  \ g\xF6r detta f\xF6r\u2026"
title: "F\xE5 det aktuella datumet"
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
