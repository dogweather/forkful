---
title:                "Att hämta aktuellt datum"
date:                  2024-01-20T15:14:40.214104-07:00
html_title:           "Bash: Att hämta aktuellt datum"
simple_title:         "Att hämta aktuellt datum"

category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hämta aktuellt datum innebär att ta reda på vad det är för dag just nu. Programmerare använder detta för att logga händelser, sätta tidsstämplar, eller hantera agenda-funktioner.

## Hur man gör:
```Haskell
import Data.Time

getCurrentDate :: IO Day
getCurrentDate = utctDay <$> getCurrentTime

main :: IO ()
main = do
    currentDate <- getCurrentDate
    putStrLn $ "Dagens datum är: " ++ show currentDate
```

Kör programmet och utdatan blir något i stil med:

```
Dagens datum är: 2023-04-12
```

## Djupdykning
I Haskell används `Data.Time` biblioteket, en del av `time` paketet skrivet av Ashley Yakeley. Det lanserades först på 2000-talet och är etablerat som standard för tidsberäkning. Alternativ som `old-time` är föråldrade och bör undvikas. När man hämtar det aktuella datumet, anropar vi `getCurrentTime` som ger oss en `UTCTime`, och sedan använder vi `utctDay` för att omvandla detta till ett `Day` objekt.

## Se även
- Haskell `time` biblioteket: http://hackage.haskell.org/package/time
- Data.Time modul: https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html
- UTCTime i Haskell: https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Clock.html#t:UTCTime
