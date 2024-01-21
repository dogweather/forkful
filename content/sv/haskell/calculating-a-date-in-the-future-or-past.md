---
title:                "Beräkna ett datum i framtiden eller förflutenheten"
date:                  2024-01-20T17:31:22.662722-07:00
model:                 gpt-4-1106-preview
simple_title:         "Beräkna ett datum i framtiden eller förflutenheten"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att räkna ut ett datum i framtiden eller förflutet innebär att man lägger till eller subtraherar tid från ett specifikt datum. Programmerare gör detta för att hantera bokningar, påminnelser, uppgiftsplanering och för att spåra tidsberoende händelser.

## Hur gör man:
För att räkna ut datumen i Haskell, använd `Data.Time` biblioteket:

```Haskell
import Data.Time

main :: IO ()
main = do
    -- Dagens datum
    today <- getCurrentTime
    let (year, month, day) = toGregorian . utctDay $ today
    putStrLn $ "Idag är det: " ++ show year ++ "-" ++ show month ++ "-" ++ show day

    -- Lägger till 10 dagar till dagens datum
    let tenDaysLater = addDays 10 (utctDay today)
    putStrLn $ "Om 10 dagar: " ++ show (toGregorian tenDaysLater)

    -- Tar bort 7 dagar från dagens datum
    let sevenDaysAgo = addDays (-7) (utctDay today)
    putStrLn $ "För 7 dagar sedan var det: " ++ show (toGregorian sevenDaysAgo)
```

Sample output:

```
Idag är det: 2023-4-5
Om 10 dagar: (2023,4,15)
För 7 dagar sedan var det: (2023,3,29)
```

## Fördjupning
Att räkna ut datum har varit en del av programmering ända sedan de första kalenderapparna på datorerna. Historiskt sett använde man enkel aritmetik eller specialbyggda algoritmer. I Haskell förenklas uppgiften med `Data.Time` biblioteket, som hanterar komplexiteter som skottår och tidszoner. Alternativ inkluderar att skriva egen kod för beräkningar eller använda andra bibliotek som `time` och `old-time`. Viktiga implementeringsdetaljer att komma ihåg är att datum och tid är lokala för användarens tidszon och det behöver ofta omvandlas till UTC för att göra korrekta beräkningar.

## Se även
- [Haskell Time library documentation](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [ZonedTime in Haskell](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-LocalTime.html)
- [Haskell Date and Time tutorial](http://learnyouahaskell.com/zippers#time-machine)