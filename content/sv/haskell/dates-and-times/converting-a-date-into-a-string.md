---
date: 2024-01-20 17:37:04.615224-07:00
description: "Att konvertera ett datum till en str\xE4ng inneb\xE4r att omvandla datatypen\
  \ `Date` till en l\xE4sbar textform. Programmerare g\xF6r detta f\xF6r att presentera\
  \ datum\u2026"
lastmod: '2024-02-25T18:49:36.265961-07:00'
model: gpt-4-1106-preview
summary: "Att konvertera ett datum till en str\xE4ng inneb\xE4r att omvandla datatypen\
  \ `Date` till en l\xE4sbar textform. Programmerare g\xF6r detta f\xF6r att presentera\
  \ datum\u2026"
title: "Omvandla ett datum till en str\xE4ng"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera ett datum till en sträng innebär att omvandla datatypen `Date` till en läsbar textform. Programmerare gör detta för att presentera datum till användare eller för att sammanställa och spara datum i textfiler eller databaser.

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
