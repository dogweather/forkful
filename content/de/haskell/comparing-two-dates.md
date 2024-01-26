---
title:                "Vergleich von zwei Daten"
date:                  2024-01-20T17:33:17.972760-07:00
model:                 gpt-4-1106-preview
simple_title:         "Vergleich von zwei Daten"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?
Vergleichen von zwei Daten bedeutet, zu prüfen, ob ein Datum vor, gleich oder nach einem anderen liegt. Programmierer benötigen das, um zeitliche Abläufe zu organisieren, Fristen zu überwachen oder historische Daten auszuwerten.

## Anleitung:
In Haskell vergleichen wir Daten mit den üblichen Vergleichsoperatoren, nachdem wir sie mit `parseTimeM` geparst haben. Hier ein kurzes Beispiel:

```haskell
import Data.Time
import Data.Time.Format

-- Zuerst beide Daten als Strings
dateStr1 = "2023-03-25"
dateStr2 = "2023-03-26"

-- Parse-Funktion, um String in ein Datum umzuwandeln
parseDate :: String -> IO Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

-- Vergleich von zwei Daten
main :: IO ()
main = do
    date1 <- parseDate dateStr1
    date2 <- parseDate dateStr2
    putStrLn $ "Datum 1 ist vor Datum 2: " ++ show (date1 < date2)
    putStrLn $ "Datum 1 ist gleich Datum 2: " ++ show (date1 == date2)
    putStrLn $ "Datum 1 ist nach Datum 2: " ++ show (date1 > date2)
```

Für das Beispiel lautet die Ausgabe:

```
Datum 1 ist vor Datum 2: True
Datum 1 ist gleich Datum 2: False
Datum 1 ist nach Datum 2: False
```

## Deep Dive:
Haskell verwendet das `Data.Time`-Modul zur Datum- und Zeitbehandlung, das mit dem Paket `time` kommt. Vor diesem Paket gab es verschiedene Alternativen und Zusatzbibliotheken, doch `time` ist inzwischen Standard.

Ein Datum in Haskell ist ein Wert des Typs `Day`, der intern als eine Anzahl von Tagen seit einer festgelegten Ära (the Modified Julian Date) gespeichert wird. Der Vergleich erfolgt daher einfach als Vergleich dieser ganzzahligen Werte.

Neben `parseTimeM` gibt es verschiedene Funktionen, um mit Datums- und Zeitwerten zu arbeiten, wie `diffDays`, um die Differenz zwischen zwei Daten zu berechnen, oder `addDays`, um eine Anzahl von Tagen zu einem Datum hinzu zuzählen.

In der Praxis könnten noch Zeitzonen und Sommerzeit eine Rolle spielen, was das comparieren komplizierter gestalten kann, hier könnte `utcToLocalTime` oder `zonedTimeToUTC` nützlich sein.

## Siehe Auch:
- [`Data.Time`-Modul](https://hackage.haskell.org/package/time-1.11.1/docs/Data-Time.html)
- [Haskell-Paket: time](https://hackage.haskell.org/package/time)
