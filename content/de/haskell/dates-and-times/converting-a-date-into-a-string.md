---
date: 2024-01-20 17:36:30.516509-07:00
description: "How to: Umwandeln von Datum zu String, das klingt simpel, hat aber seine\
  \ T\xFCcken. In Haskell ist `Data.Time` die zentrale Bibliothek f\xFCr Datum und\
  \ Zeit.\u2026"
lastmod: '2024-04-05T22:51:08.499031-06:00'
model: gpt-4-1106-preview
summary: "Umwandeln von Datum zu String, das klingt simpel, hat aber seine T\xFCcken."
title: Datum in einen String umwandeln
weight: 28
---

## How to:
```Haskell
import Data.Time

-- Beispiel: Aktuelles Datum in einen String umwandeln
main :: IO ()
main = do
    currentDay <- getCurrentTime
    let dateString = formatTime defaultTimeLocale "%Y-%m-%d" currentDay
    putStrLn dateString
```
Beispiel-Ausgabe:
```plaintext
2023-03-14
```

## Deep Dive
Umwandeln von Datum zu String, das klingt simpel, hat aber seine Tücken. In Haskell ist `Data.Time` die zentrale Bibliothek für Datum und Zeit. Historisch basierte das Formatieren auf der C-Bibliothek strftime, daher die ähnlichen Format-Spezifikatoren. Alternativen zur Standardbibliothek sind Pakete wie `time-fmt`, die eine einfachere API bieten können.

Haskells Typensystem sorgt für klare Implementationen: `UTCTime` für Zeiten in UTC, `LocalTime` für lokale Zeiten, abhängig von einer Zeitzone. Beachten sollte man die TimeLocale, welche die lokalen Einstellungen bei der Formatierung nutzt – wichtig für die Internationalisierung.

## Siehe Auch
- Haskell `Data.Time` Modul: https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html
- `time-fmt` Paket: https://hackage.haskell.org/package/time-fmt
- strftime Format-Spezifikatoren: http://strftime.org/
