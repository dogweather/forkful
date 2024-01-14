---
title:                "Haskell: Ein Datum in einen String konvertieren"
simple_title:         "Ein Datum in einen String konvertieren"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren eines Datums in eine Zeichenkette kann in vielen verschiedenen Situationen nützlich sein, z. B. beim Erstellen von Berichten, beim Exportieren von Daten oder einfach nur bei der Anzeige von Datum und Uhrzeit in einem lesbareren Format.

## Wie geht das?

Die Konvertierung eines Datums in eine Zeichenkette ist in Haskell relativ einfach. Wir können die Funktion "show" verwenden, um das Datum in das Datumsformat "String" umzuwandeln. Hier ist ein Beispielcode:

```Haskell
import Data.Time

-- Ein Datum erstellen
let date = fromGregorian 2020 01 01

-- Datum in String konvertieren
let dateString = show date 

-- Ausgabe: "2020-01-01"
print dateString
```

Wie Sie sehen können, wird das Datum im Format "YYYY-MM-DD" angezeigt. Dies ist die Standardformatierung für Datumswerte in Haskell.

Natürlich können wir die Ausgabe auch in einem anderen Format erhalten. Hier ist ein Beispiel, bei dem wir das Datum im Format "TT.MM.JJJJ" anzeigen lassen:

```Haskell
-- Datum in String konvertieren mit angegebenem Format
let dateString = formatTime defaultTimeLocale "%d.%m.%Y" date 

-- Ausgabe: "01.01.2020"
print dateString
```

In diesem Beispiel verwenden wir die Funktion "formatTime" und geben das gewünschte Format als Argument an. Wir können auch weitere Details wie die Uhrzeit oder die Zeitzone hinzufügen, indem wir das Format entsprechend anpassen.

## Tiefergehende Informationen

Das Datumskonvertierungsmodul in Haskell, "Data.Time", bietet viele weitere Funktionen und Möglichkeiten, um Datums- und Zeitinformationen zu manipulieren. Es gibt zum Beispiel Funktionen, um Zeiträume zu berechnen, Zeitzonen zu konvertieren oder Tageslichtzeiten zu berücksichtigen. Eine ausführliche Dokumentation zu diesen Funktionen und mehr finden Sie in der offiziellen Haskell-Dokumentation für das Datumskonvertierungsmodul: [https://hackage.haskell.org/package/time](https://hackage.haskell.org/package/time)

## Siehe auch

- [Haskell-Dokumentation zum Datumskonvertierungsmodul](https://hackage.haskell.org/package/time)
- [Tutorial zu Datumsmanipulation in Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/10-Dates-and-Calendars)
- [Weitere Infos zu Zeit- und Datumskonvertierung in Haskell](https://www.4haskell.com/2019/07/date-translation-and-manipulation