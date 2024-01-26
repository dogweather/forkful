---
title:                "Tolka ett datum från en sträng"
date:                  2024-01-20T15:36:51.322713-07:00
html_title:           "Bash: Tolka ett datum från en sträng"
simple_title:         "Tolka ett datum från en sträng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka ett datum från en sträng handlar om att konvertera text till ett datumformat som programmet kan förstå och jobba med. Det behövs för att hantera datum som användare mata in eller för att läsa data som är lagrad i textformat.

## Så Här Gör Du:
Haskell har modulen `Data.Time.Format` för att hantera datum. Använd `parseTimeM` för att tolka strängar till `UTCTime` eller `ZonedTime`.

```Haskell
import Data.Time
import Data.Time.Format (parseTimeM, defaultTimeLocale)

-- Antag att vi har strängen "2023-03-21" och vi vill ha ett datum
parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

main :: IO ()
main = do
    let dateString = "2023-03-21"
    print $ parseDate dateString  -- Skriver ut: Just 2023-03-21
```

Observera att `parseTimeM` är polymorfisk, så du måste specificera returtypen (`Maybe Day` här).

## Djupdykning:
Att tolka datum från strängar har varit viktigt sedan datorprogrammering började. Datum och tid är grundläggande i många applikationer, från filsystem till databaser.

Innan `Data.Time.Format`, användes ofta `old-time` paketet i Haskell, men nu anses `Data.Time` vara mer robust och flexibel.

Alternativ för datumtolkning i Haskell inkluderar:

- Att använda biblioteket `time` för noggranna tidsberäkningar med tidszoner.
- `thyme` biblioteket för en snabbare, men mindre exakt, version av `time`.

Implementeringsdetaljerna i `parseTimeM` baseras på en specifikation av datum- och tidsformatsträngar. `%Y-%m-%d` är ett sådant format som står för "År-Månad-Dag".

## Se Också:
- Haskell `time` paket: https://hackage.haskell.org/package/time
- Dokumentation för `Data.Time.Format`: https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html
- Förstå tid och datum i Haskell: https://www.schoolofhaskell.com/user/dmchess/time
- `thyme` biblioteket på Hackage: https://hackage.haskell.org/package/thyme
