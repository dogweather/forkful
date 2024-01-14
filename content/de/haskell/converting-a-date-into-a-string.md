---
title:                "Haskell: Converting eines Datums in eine Zeichenkette."
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Die Konvertierung von Datum in eine Zeichenfolge ist ein wichtiger Schritt in der Programmierung, da sie es ermöglicht, Daten in einem leicht lesbaren Format auszugeben. Dies kann beispielsweise bei der Erstellung von Berichten oder beim Speichern von Daten in einer Datei hilfreich sein.

## Wie man das macht

Um in Haskell ein Datum in eine Zeichenfolge umzuwandeln, können wir die Funktion `show` verwenden. Diese Funktion akzeptiert ein Datum als Argument und gibt eine Zeichenfolge zurück, die das Datum im Format "JAHR-MONAT-TAG" darstellt. Schauen wir uns ein Beispiel an:

```Haskell
show (2021, 10, 26)
```
Ausgabe: "2021-10-26"

Hier ist ein Beispiel, wie du die Funktion `show` in einer Funktion verwenden kannst, die das aktuelle Datum als Zeichenfolge ausgibt:

```Haskell
import Data.Time

getToday :: IO String
getToday = do
    t <- getZonedTime
    return (show $ localDay $ zonedTimeToLocalTime t)
```
Ausgabe (je nach aktuellem Datum): "2021-10-26"

Es gibt auch andere Funktionen, die verwendet werden können, um das Format der Ausgabe zu ändern, zum Beispiel die Funktionen `formatTime` und `formatTimeLocale`.

## Tiefergehende Informationen

In Haskell gibt es verschiedene Datentypen, die Datum und Uhrzeit repräsentieren, wie zum Beispiel `UTCTime`, `ZonedTime` und `LocalTime`. Wenn du dich intensiver mit der Konvertierung von Datum in eine Zeichenfolge beschäftigen möchtest, solltest du dich mit diesen Datentypen vertraut machen.

Ein weiterer wichtiger Aspekt bei der Konvertierung von Datum in eine Zeichenfolge ist die Lokalisierung. Je nach Region oder Sprache können die Formate variieren. Dafür bietet Haskell die Funktion `setLocaleTime` an, mit der du das Ausgabeformat anpassen kannst.

## Siehe auch
- [Haskell-Dokumentation zu `show`](https://www.haskell.org/onlinereport/standard-prelude.html#show)
- [Hackage-Dokumentation zu Datum und Uhrzeit in Haskell](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Tutorial: Datum und Uhrzeit in Haskell](https://www.tutorialspoint.com/haskell/haskell_date_time.htm)