---
title:    "Haskell: Omvandla ett datum till en sträng"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Varför

Att konvertera ett datum till en sträng kan vara en viktig och användbar funktion i Haskell-programmering. Det kan tillåta dig att hantera datum och tid i ett läsbart format och använda dem på olika sätt inom ditt program.

## Hur man gör

Det finns flera olika sätt att konvertera ett datum till en sträng i Haskell. Ett sätt är att använda det inbyggda Date.Time-paketet.

```Haskell
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (getCurrentTime)

getCurrentDate :: IO String
getCurrentDate = do
  time <- getCurrentTime
  return $ formatTime defaultTimeLocale "%d/%m/%Y" time

main :: IO ()
main = do
  currentDate <- getCurrentDate
  putStrLn $ "Idag är datumet: " ++ currentDate
  
-- Output:
-- Idag är datumet: 22/02/2021
```

Här använder vi getCurrentTime för att hämta det aktuella datumet och därefter använder vi formatTime för att ange önskat datumformat. I detta fall väljer vi "%d/%m/%Y" för att få datumet i formatet dag/månad/år.

En annan metod är att använda funktionen showDate från paketet Data.Time.Calendar. Detta ger dock bara datumet utan tidsinformation.

```Haskell
import Data.Time.Calendar (showDate)

main :: IO ()
main = do
  let date = showDate (2021, 2, 22)
  putStrLn date
  
-- Output:
-- 2021-02-22
```

## Fördjupning

Genom att använda paketet Date.Time kan du konvertera datum till strängar i olika format. Det finns också andra funktioner som kan användas för att manipulera, jämföra och arbeta med datum och tider i Haskell.

För mer information om Date.Time-paketet och hur man kan använda det för att konvertera datum till strängar, se följande länkar:

- https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html
- https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Clock.html
- https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Calendar.html

## Se även

- https://www.haskell.org/
- https://www.haskell.org/documentation/