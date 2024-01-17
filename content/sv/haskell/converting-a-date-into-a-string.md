---
title:                "Omvandla ett datum till en sträng"
html_title:           "Haskell: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att konvertera ett datum till en sträng är en vanlig uppgift inom programmering. Det betyder helt enkelt att vi vill omvandla ett datum till en läslig text i strängformat. Detta är en användbar funktion eftersom det tillåter program att utdata datum på ett förståeligt sätt för användare.

## Hur man gör det:

```Haskell
import Data.Time.Format

-- Skapa ett datum
let datum = fromGregorian 2019 11 8

-- Konvertera datumet till en sträng med formatet DD.MM.YYYY
let datumSträng = formatTime defaultTimeLocale "%d.%m.%Y" datum

-- Skriv ut resultatet
datumSträng
-- Output: "08.11.2019"

```

## Djupdykning:

Konvertering av datum till strängar är en viktig del av en programmers verktygslåda. Detta görs ofta för att möjliggöra utdata i ett visst format, eller för att underlätta användarens läsning av datum. Andra alternativ för datumkonvertering inkluderar att skapa egna funktioner för att formatera datumet eller använda inbyggda bibliotek som `Data.Time.Format`.

En intressant faktum är att en stor del av datumen som vi använder idag är baserade på den Gregorianska kalendern, som infördes av påven Gregorius XIII år 1582. Detta har blivit den standardkalendern som används i stora delar av världen idag.

## Se även:

- [Haskell.org](https://www.haskell.org/) - officiell hemsida för Haskell
- [Data.Time.Format modul](https://www.haskell.org/hoogle/?hoogle=Data.Time.Format) - dokumentation för `Data.Time.Format` biblioteket i Haskell