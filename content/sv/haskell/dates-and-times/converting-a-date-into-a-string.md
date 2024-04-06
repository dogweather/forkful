---
date: 2024-01-20 17:37:04.615224-07:00
description: "Hur g\xF6r man: Exempel p\xE5 utdata."
lastmod: '2024-04-05T21:53:39.306383-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Omvandla ett datum till en str\xE4ng"
weight: 28
---

## Hur gör man:
```Haskell
import Data.Time

-- Exempel på att formatera dagens datum som en sträng
main :: IO ()
main = do
    currentDay <- getCurrentTime
    let dateString = formatTime defaultTimeLocale "%Y-%m-%d" currentDay
    putStrLn dateString
```
Exempel på utdata:
```
2023-03-15
```

## Fördjupning:
Att hantera datum och tider i Haskell är rakt på sak med `Data.Time`-biblioteket, som har blivit standard sedan dess introduktion. Alternativ till `Data.Time` inkluderar äldre bibliotek som `Time` och tredjepartspaket, men `Data.Time` erbjuder bäst integration och flexibilitet. Det stödjer många tidszoner och tidräkningssystem.

Basen för datumsträngskonvertering ligger i `formatTime`-funktionen som tar en lokalisering, ett format och ett datum för att producera en sträng. Formatsträngen är kraftfull, den tillåter definiering av precis vilken datumsstruktur du vill ha. Tänk på att inte alla länder använder samma datumformat – att välja rätt format är nödvändigt för internationellisering.

## Se även:
- Haskell `Data.Time`-modul: [https://hackage.haskell.org/package/time-1.11.1.2/docs/Data-Time.html](https://hackage.haskell.org/package/time-1.11.1.2/docs/Data-Time.html)
- Haskell `formatTime`-funktionsdokumentation: [https://hackage.haskell.org/package/time-1.11.1.2/docs/Data-Time-Format.html#v:formatTime](https://hackage.haskell.org/package/time-1.11.1.2/docs/Data-Time-Format.html#v:formatTime)
