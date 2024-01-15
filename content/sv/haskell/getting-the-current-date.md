---
title:                "Att få den aktuella datumet."
html_title:           "Haskell: Att få den aktuella datumet."
simple_title:         "Att få den aktuella datumet."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför
Att kunna hämta den aktuella datum är en viktig funktion som kan hjälpa oss att hålla koll på tiden eller till exempel användas i applikationer för att skapa dynamiska funktioner baserat på aktuell tid.

## Så här gör du
```Haskell
import Data.Time.Clock
import Data.Time.Calendar

-- Hämta den aktuella tidpunkten
getCurrTime = getCurrentTime

-- Hämta den aktuella datumet
getCurrDate = getCurrentDay

-- Sample output
> getCurrTime
2021-04-10 12:00:00 UTC
> getCurrDate
2021-04-10
```

## Djupdykning
För att hämta den aktuella tidpunkten använder vi funktionen `getCurrentTime` från `Data.Time.Clock` biblioteket. Denna funktion returnerar ett `UTCTime` objekt som innehåller all information om tidpunkten, inklusive datum.

För att få bara datumet, använder vi funktionen `getCurrentDay` från `Data.Time.Calendar` biblioteket. Denna funktion returnerar ett `Day` objekt som bara innehåller datumet utan tidsinformation.

## Se även
- [Haskell Official Documentation for Data.Time.Clock] (https://www.haskell.org/cabal/users-guide/developing-packages.html#handling-time)
- [Haskell Official Documentation for Data.Time.Calendar] (https://www.haskell.org/cabal/users-guide/developing-packages.html#handling-time)