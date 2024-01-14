---
title:                "Haskell: Beräkna ett datum i framtiden eller förflutna"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Varför

Att beräkna ett datum i framtiden eller dåtid kan vara användbart för många olika programmeringsprojekt, inklusive tidshantering, schemaläggning och kalenderapplikationer.

# Hur man gör

För att göra detta i Haskell finns det flera olika sätt att närma sig det. Ett sätt är att använda biblioteket "time" som innehåller funktioner för att hantera datum och tid. Nedanstående kodexempel visar hur man beräknar ett datum 30 dagar framåt från nuvarande datum:

```Haskell
import Data.Time
import Data.Time.Calendar.OrdinalDate

futureDate = addDays 30 $ utctDay $ getCurrentTime
print $ futureDate

-- Output: 2021-12-31
```

Dessa funktioner gör det enkelt att manipulera datum i olika format och ta hänsyn till skottår. Om du vill beräkna ett datum i förfluten tid kan du göra det genom att använda "diffDays" funktionen istället för "addDays".

# Djupdykning

Vissa projekt kan kräva mer avancerad beräkning av datum, som till exempel att ta hänsyn till skiftande tidszoner eller att hantera speciella händelser som sommartid. För dessa ändamål finns det andra bibliotek som erbjuder mer robust funktionalitet. Det är viktigt att välja rätt bibliotek beroende på dina behov och krav.

# Se även

- [Officiell dokumentation för "time" biblioteket](https://hackage.haskell.org/package/time)
- [Mer information om hantering av datum och tid i Haskell](https://wiki.haskell.org/Time_Standard_2)
- [En sammanfattning av de bästa Haskell biblioteken för beräkning av datum](https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/haskells-date-library/)