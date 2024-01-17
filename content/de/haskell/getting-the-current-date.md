---
title:                "Den aktuellen Datum erhalten"
html_title:           "Haskell: Den aktuellen Datum erhalten"
simple_title:         "Den aktuellen Datum erhalten"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums ist eine grundlegende Funktion in der Programmierung, die es ermöglicht, das heutige Datum in einem bestimmten Format zu erhalten. Programmierer verwenden diese Funktion oft, um Zeitstempel für Dateien zu erstellen, Berichte zu generieren oder die Dauer zwischen zwei bestimmten Daten zu berechnen.

## Wie geht's?
Das Abrufen des aktuellen Datums in Haskell ist einfach und erfordert nur eine kurze Codezeile:
```Haskell
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)

getCurrentDate :: IO String
getCurrentDate = getCurrentTime >>= \time -> return (formatTime defaultTimeLocale "%d.%m.%Y" time)
```
Der obige Code importiert die Data.Time-Bibliothek und definiert eine Funktion namens `getCurrentDate`. Diese Funktion ruft die aktuelle Zeit mit `getCurrentTime` ab und formatiert sie dann mit `formatTime` im gewünschten Format. Schließlich gibt die Funktion das formatierte Datum als String mit dem gewünschten Format (in diesem Fall DD.MM.JJJJ) zurück.

Das Ergebnis des obigen Codes kann folgendermaßen aussehen, abhängig vom aktuellen Datum:
```
23.07.2020
```

## Tiefere Einblicke
Die Funktion `getCurrentTime` bietet eine höhere Genauigkeit als eine einfache Datumsabfrage, da sie auch die Zeit mit Millisekunden zurückgibt. Das `formatTime`-Funktion verwendet das `defaultTimeLocale`, um das Datum im gewünschten Format anzuzeigen. Es gibt jedoch auch andere Möglichkeiten, das Datum zu formatieren, z.B. die Verwendung des `localTimeFormat` oder die Konvertierung des Datums in einen `UTCTime` Wert.

Eine Alternative zum Abrufen des aktuellen Datums ist die Verwendung der `Data.Time.Clock` Bibliothek, die mehr Funktionen für die Manipulation von Zeitwerten bietet.

## Siehe auch
Weitere Informationen zu den oben erwähnten Funktionen und Bibliotheken finden Sie in der offiziellen Dokumentation für die Data.Time und Data.Time.Clock Module.

[Dokumentation für Data.Time](https://hackage.haskell.org/package/time-1.9/docs/Data-Time.html)

[Dokumentation für Data.Time.Clock](https://hackage.haskell.org/package/time-1.9/docs/Data-Time-Clock.html)