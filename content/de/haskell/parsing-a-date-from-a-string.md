---
title:                "Einen Datum aus einem String parsen"
html_title:           "Elixir: Einen Datum aus einem String parsen"
simple_title:         "Einen Datum aus einem String parsen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen eines Datums aus einem String bezieht sich auf den Prozess der Umwandlung eines vorgegebenen String-Eintrags in ein Datum oder eine Uhrzeit. Programmierer tun dies, um das Datumsformat zu standardisieren und um zeitbezogene Berechnungen und Operationen durchführen zu können.

## Wie Macht Man Das:

Hier ist ein einfaches Beispiel, wie man in Haskell ein Datum aus einem String parst. Wir verwenden die `parseTimeM` Funktion aus dem `Data.Time.Format` Modul:

```Haskell
import Data.Time
import System.Locale

parseDateString :: String -> Maybe UTCTime
parseDateString = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z" 

main :: IO ()
main = do
  print $ parseDateString "2021-02-26T14:12:36UTC"
```

Und hier ist die Ausgabe des oben genannten Haskell-Codes:

```Haskell
Just 2021-02-26 14:12:36 UTC
```

## Vertiefung:

Das Parsen eines Datums aus einem String ist oft erforderlich, da Daten gelegentlich in Textform anstatt als spezialisierte Datentypen vorliegen. Die Implementierung in Haskell ist besonders elegant, da sie einen funktionalen Ansatz nutzt und Musterabgleich verwendet.

Es gibt auch Alternativen zur `parseTimeM` Funktion. Sie könnten beispielsweise die Funktionen `read` oder `reads` verwenden, wenn Sie wissen, dass das Format immer korrekt ist und Sie keine Fehlerbehandlung benötigen.

In Bezug auf Implementierungsdetails verwendet `parseTimeM` eine Monade, um das Ergebnis zu verarbeiten. Im Fall eines Parse-Fehlers gibt die Funktion `Nothing` zurück, und wenn das Parsen erfolgreich ist, erhält man `Just parsedTime`.

## Siehe Auch:

- Die Dokumentation zur `Data.Time` Bibliothek finden Sie [hier](https://hackage.haskell.org/package/time-1.5/docs/Data-Time.html).
- Eine ausführliche Anleitung zur Haskell-Programmiersprache finden Sie auf der offiziellen [Haskell-Website](https://www.haskell.org/).
- Um mehr zu erfahren, sehen Sie sich dieses Tutorial zur [Haskell-Datums- und Zeitverarbeitung](https://williamyaoh.com/posts/2020-07-12-dates-and-time-in-haskell.html) an.