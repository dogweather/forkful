---
title:                "Analyse eines Datums aus einem String"
html_title:           "Haskell: Analyse eines Datums aus einem String"
simple_title:         "Analyse eines Datums aus einem String"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String ist eine nützliche Fähigkeit für Programmierer, da es es ihnen ermöglicht, Datumsangaben aus Benutzereingaben oder externen Dateien zu lesen und in ein geeignetes Dateiformat umzuwandeln. Dies kann besonders hilfreich sein, wenn Programme mit verschiedenen Datumsformaten arbeiten müssen.

## So geht's:
Haskell bietet mit der Funktion "parseTimeM" eine einfache Möglichkeit, um ein Datum aus einem String zu parsen. Hier ein Beispiel, um ein Datum im Format "Tag-Monat-Jahr" zu parsen und in das ISO-8601-Format umzuwandeln:

```Haskell
import Data.Time.Format
import Data.Time.Clock
import System.Locale

parseDate :: String -> IO ()
parseDate datestr = do
    let format = "%d-%m-%Y"
    parsed <- parseTimeM True defaultTimeLocale format datestr :: Maybe UTCTime
    case parsed of
        Just date -> putStrLn $ "Parsed date: " ++ show date
        Nothing -> putStrLn "Invalid date format."
```

Wenn wir diese Funktion mit dem Argument "23-11-2021" aufrufen, erhalten wir die Ausgabe "Parsed date: 2021-11-23 00:00:00 UTC".

## Tiefseetauchen:
Das Parsen von Daten aus Strings ist ein wesentlicher Bestandteil der Verarbeitung von Benutzereingaben und Daten aus externen Quellen. Es ermöglicht Programmen, flexibel auf verschiedene Datumsformate zu reagieren und ihnen ein einheitliches Format aufzuzwingen.

Es gibt verschiedene Alternativen für das Parsen von Daten in Haskell, wie z.B. das Paket "time" oder die Funktion "readTime" aus dem Paket "Text.Read". Beide bieten ähnliche Funktionalitäten, haben aber geringfügige Unterschiede in der Syntax.

Die "parseTimeM" Funktion verwendet das "ParseTime" Typenklasse, die es ermöglicht, eigene Datenstrukturen für spezifische Datumsformate zu definieren. So kann man z.B. eine Instanz für das Jahr 2021 erstellen, die automatisch das richtige Format erkennt und parsen kann.

## Siehe auch:
- [Haskell-Dokumentation zu "parseTimeM"](https://hackage.haskell.org/package/time/docs/Data-Time-Format-ISO8601.html#v:parseTimeM)
- [Alternative "time" Paket](https://hackage.haskell.org/package/time)
- [Alternative "Text.Read" Paket](https://hackage.haskell.org/package/text-docs-1.2.4.0/docs/Text-Read.html#v:readTime)