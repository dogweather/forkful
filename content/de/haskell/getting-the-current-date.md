---
title:                "Haskell: Die aktuelle Datum erhalten"
simple_title:         "Die aktuelle Datum erhalten"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man das aktuelle Datum in Haskell abrufen möchte. Vielleicht möchtest du eine Funktion schreiben, die das Datum verwendet, um verschiedene Aufgaben auszuführen, wie zum Beispiel das Erstellen von Dateinamen oder das Überprüfen, ob ein bestimmtes Datum in der Zukunft liegt. Oder du möchtest einfach nur aus Interesse sehen, wie Haskell das aktuelle Datum handhabt. In jedem Fall ist es wichtig zu wissen, wie man das aktuelle Datum in Haskell abrufen kann.

## Wie geht's

Die aktuelle Datum kann in Haskell auf verschiedene Weise abgerufen werden. Hier sind zwei gängige Methoden:

```Haskell
import Data.Time
import Data.Time.Format
import Data.Time.Clock

-- Methode 1: mithilfe der getCurrentTime-Funktion
-- Output: 2019-09-17 14:30:00.112856 UTC
main = do
    currentTime <- getCurrentTime
    print currentTime

-- Methode 2: mithilfe der getCurrentTime-Funktion und Formatierung mit strftime
-- Output: "17.09.19, 14:30:00"
main = do
    currentTime <- getCurrentTime
    let formattedTime = formatTime defaultTimeLocale "%d.%m.%y, %H:%M:%S" currentTime
    print formattedTime
```

## Tiefere Einblicke

Die Verwendung von `getCurrentTime` gibt uns ein `UTCTime`-Objekt, das die Uhrzeit im UTC-Format beinhaltet. Um es in ein benutzerfreundlicheres Format zu bringen, können wir es mit `formatTime` und einer entsprechenden Formatierungsfunktion wie `defaultTimeLocale` konvertieren. Alternativ können andere Funktionen wie `localTime` und `getCurrentTimeZone` verwendet werden, um die aktuelle lokale Uhrzeit zu erhalten.

Es ist auch zu beachten, dass `getCurrentTime` eine IO-Aktion ist, was bedeutet, dass sie Seiteneffekte haben kann, wie zum Beispiel die Verbindung zum Internet zur Synchronisierung der Uhrzeit. Aus diesem Grund wird in der zweiten Methode die Uhrzeit in einer separaten Variable gespeichert und dann mit `print` ausgegeben, um mögliche unerwünschte Seiteneffekte zu vermeiden.

Insgesamt bietet Haskell viele verschiedene Möglichkeiten, das aktuelle Datum zu erhalten, je nach den Anforderungen deines Codes.

## Siehe auch

- [Dokumentation zu Data.Time-Modul](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Beispiele für Datumsformatierung](https://hackage.haskell.org/package/time/docs/Data-Time-Format.html#g:2)
- [Stack Overflow-Fragen zu aktuellen Dateien in Haskell](https://stackoverflow.com/questions/3916065/how-to-get-the-current-time-in-hour-minute-second-format)