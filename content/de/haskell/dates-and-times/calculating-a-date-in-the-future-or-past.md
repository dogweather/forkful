---
date: 2024-01-20 17:31:01.920677-07:00
description: "Das Berechnen eines Datums in der Zukunft oder der Vergangenheit ist\
  \ einfach, bestimmt wann Dinge passiert sind oder passieren werden. Programmierer\u2026"
lastmod: '2024-03-11T00:14:27.841618-06:00'
model: gpt-4-1106-preview
summary: "Das Berechnen eines Datums in der Zukunft oder der Vergangenheit ist einfach,\
  \ bestimmt wann Dinge passiert sind oder passieren werden. Programmierer\u2026"
title: "Berechnung eines zuk\xFCnftigen oder vergangenen Datums"
---

{{< edit_this_page >}}

## Was & Warum?
Das Berechnen eines Datums in der Zukunft oder der Vergangenheit ist einfach, bestimmt wann Dinge passiert sind oder passieren werden. Programmierer nutzen das für Features wie Erinnerungen, Zeitplanung oder Gültigkeitsprüfungen.

## How to:
```Haskell
import Data.Time

-- Ein Datum in der Zukunft berechnen
addDaysToCurrent :: Integer -> IO Day
addDaysToCurrent n = do
    today <- utctDay <$> getCurrentTime
    return $ addDays n today

-- Ein Datum in der Vergangenheit berechnen
subtractDaysFromCurrent :: Integer -> IO Day
subtractDaysFromCurrent n = addDaysToCurrent (-n)

-- Beispiel Nutzung
main :: IO ()
main = do
    futureDate <- addDaysToCurrent 10
    putStrLn $ "In 10 Tagen wird es der " ++ show futureDate ++ " sein."
    
    pastDate <- subtractDaysFromCurrent 10
    putStrLn $ "Vor 10 Tagen war es der " ++ show pastDate ++ "."
```

Sample Output:
```
In 10 Tagen wird es der 2023-04-30 sein.
Vor 10 Tagen war es der 2023-04-10.
```

## Deep Dive
In Haskell berechnen wir Daten mit der Bibliothek 'Data.Time'. Sie ist robust und dabei weit verbreitet. Historisch gesehen war das Rechnen mit Zeiten schon immer wichtig, für Kalender oder Astronomie. Andere Programmiersprachen bieten ähnliche Bibliotheken; in Java wäre das `java.time`, in Python `datetime`. 

In der Implementierung kümmert sich 'Data.Time' um Schaltjahre, verschiedene Kalendersysteme und Zeitberechnungen. Es benutzt 'UTCTime' für die aktuelle Zeit, welches Zeitzonen-unabhängig ist und dann konvertiert es das zur lokalen Zeitzone wenn nötig.

## See Also
- Das `Data.Time` Paket auf Hackage: [Data.Time](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- Eine tiefere Einführung in `Data.Time`: [Haskell Data.Time tutorial](https://two-wrongs.com/haskell-time-library-tutorial)
- ISO 8601 Date and Time Formats (für Standardisierung): [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html)
- Haskell Programming from first principles: [HaskellBook](http://haskellbook.com/)
