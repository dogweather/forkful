---
title:    "Haskell: Das aktuelle Datum erhalten"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das Abrufen des aktuellen Datums ist eine häufige Aufgabe in der Programmierung. Es kann nützlich sein, um bestimmte Funktionen abhängig vom Datum oder zur Erstellung von Zeitstempeln zu steuern.

## Wie

Um das aktuelle Datum in Haskell abzurufen, müssen wir zunächst die `Data.Time` Bibliothek importieren. Dann können wir die `getCurrentTime` Funktion aufrufen, die uns ein `UTCTime` Objekt zurückgibt.

```Haskell
import Data.Time

currentDate :: IO UTCTime
currentDate = getCurrentTime
```

Dieses Objekt enthält das aktuelle Datum und die Uhrzeit in der koordinierten Weltzeit (UTC). Wenn wir das Datum in einem anderen Zeitformat anzeigen möchten, können wir die `formatTime` Funktion verwenden.

```Haskell
import Data.Time.Format

dateString :: String
dateString = formatTime defaultTimeLocale "%d.%m.%Y" currentDate
```

Dieser Code würde uns das Datum im Format "DD.MM.YYYY" als String zurückgeben. Wir können auch die `getCurrentTime` Funktion mit einem `TimeZone` Argument aufrufen, um das Datum in einer bestimmten Zeitzone zu erhalten.

## Tiefgehende Einblicke

Die `getCurrentTime` Funktion ruft tatsächlich die aktuelle Systemzeit vom Betriebssystem ab. Dies wird als "IO-Aktion" bezeichnet, da es eine Aktion im Eingabe/Ausgabe-System ist. Dies bedeutet, dass das Abrufen des aktuellen Datums immer eine geringe Wahrscheinlichkeit hat, dass es fehlschlägt oder sich verzögert, da es vom Betriebssystem abhängig ist. Es ist wichtig, dies bei der Verwendung des aktuellen Datums in kritischen Anwendungen zu berücksichtigen.

## Siehe auch

- Offizielle Dokumentation zu `Data.Time`: https://hackage.haskell.org/package/time/docs/Data-Time.html
- Ein Tutorial zum Umgang mit Datum und Zeit in Haskell: https://wiki.haskell.org/Time_and_Date_Library
- Weitere Ressourcen und Codebeispiele für die Arbeit mit Datum und Zeit: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/date-and-time