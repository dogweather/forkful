---
title:                "Hämta aktuellt datum"
html_title:           "Arduino: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hämta det aktuella datumet i programmering innebär att få den pågående datuminformationen från systemet. Det används ofta för att logga när händelser sker, tidstämpla data eller generera gränssnitt för användardatum.

## Hur till:
Att få det aktuella datumet i Haskell kan enkelt uppnås genom Data.Time-paketet. Här är ett exempel:

```Haskell
import Data.Time

main = do
  date <- getCurrentTime
  print date
```

Kör den här koden, och utmatningen kommer att vara det aktuella datumet och tiden, till exempel "2023-01-24 10:20:30.52 UTC".

## Djup dykning:
Få datumhistoriken spårades tillbaka till de tidiga dagarna av programmering där tid och datum ofta behövdes för att logga händelser. 

Alternativt, om du vill hämta specifik datuminformation (som år, månad eller dag), kan du använda funktioner som toGregorian och fromGregorian i Data.Time-paketet.

Med avseende på implementeringsdetaljer, under huven, använder Data.Time-paketet UNIX tid, vilket är numret av sekunder som har gått sedan den 1 januari 1970.

## Se också: 
- Official Haskell Library: Data.Time Package https://hackage.haskell.org/package/time-1.11.1.1/docs/Data-Time.html
- Tutorial: Tids- och datumhantering i Haskell https://wiki.haskell.org/Tutorial#Time_and_date_management
- Guide: Arbeta med datum och tid i Haskell https://two-wrongs.com/haskell-time-library-tutorial