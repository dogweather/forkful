---
title:                "Aktuelles Datum abrufen"
date:                  2024-01-20T15:14:50.122659-07:00
html_title:           "C: Aktuelles Datum abrufen"
simple_title:         "Aktuelles Datum abrufen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums in Haskell bedeutet, das aktuelle Datum aus dem laufenden System zu ermitteln. Programmierer nutzen diese Funktion für alles Mögliche, von der Dateiverwaltung und Logbuchführung bis hin zur zeitgesteuerten Ausführung von Aktionen.

## How to:
Hier ein kleines Beispiel, wie du das aktuelle Datum in Haskell bekommst:

```haskell
import Data.Time

main :: IO ()
main = do
    currentDay <- getCurrentTime
    print $ utctDay currentDay
```

Ausgabe könnte so aussehen:

```
2023-04-05
```

Wenn du nur das Datum ohne die Uhrzeit willst:

```haskell
import Data.Time

main :: IO ()
main = do
    currentDate <- fmap utctDay getCurrentTime
    print currentDate
```

Ausgabe:

```
2023-04-05
```

## Deep Dive
Historisch gesehen, war das Verarbeiten von Datums- und Zeitangaben in den meisten Programmiersprachen nicht immer direkt möglich, weshalb externe Bibliotheken oder eigene Implementierungen benötigt wurden. In Haskell ist das Paket `Data.Time` Teil des `time`-Pakets, für das viele Gemeinschaftsmitglieder beigetragen haben.

Alternativen zu `Data.Time` gibt es nicht viele, aber man könnte auch Systembefehle aufrufen oder andere externe Pakete suchen. Dennoch ist `Data.Time` die gängigste und empfohlene Methode in Haskell.

Die Implementierung nutzt UTC (Coordinated Universal Time), was eine Vereinheitlichung bedeutet, aber denk dran: wenn du lokale Zeiten oder Zeitstempel brauchst, solltest du Funktionen wie `getCurrentTimeZone` und `utcToLocalTime` beachten.

## See Also
Weitere Infos zum `time`-Paket findest du direkt auf Hackage, dem Haskell-Paketarchiv:

- Hackage `time` package: https://hackage.haskell.org/package/time
- Einen Guide zur Nutzung von Datums- und Zeitfunktionen in Haskell bietet der Chapter "Dates and Times" aus dem Buch "Real World Haskell": http://book.realworldhaskell.org/read/dates-and-times.html

Für Tutorials und eine interaktive Lernumgebung kann 'Learn You a Haskell for Great Good!' hilfreich sein:

- Learn You a Haskell (deutsche Version): http://learnyouahaskell.com/chapters
