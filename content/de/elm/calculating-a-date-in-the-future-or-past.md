---
title:                "Elm: Eine Datumswertung in der Zukunft oder Vergangenheit berechnen"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von zukünftigen oder vergangenen Datums ist eine wichtige Funktion in der Programmierung. Es ermöglicht uns, Termine und Fristen zu planen und zu verwalten, sowie zeitbezogene Aufgaben und Aktionen zu automatisieren.

## Wie geht's

Die Elm Sprache bietet eine einfache und effiziente Möglichkeit, um Datumsberechnungen durchzuführen. Hier ist ein Beispiel, wie man ein zukünftiges Datum um eine bestimmte Anzahl von Tagen berechnen kann:

```Elm
import Time exposing (..)
import Date exposing (..)

let
    datum = Date.fromParts 2020 5 20
    zukuenftigesDatum = Date.add (days 10) datum
in
    text (Date.toString zukuenftigesDatum)
```

Dieses Beispiel verwendet die Module "Time" und "Date" von Elm, um ein Datum zu erstellen und dann mithilfe der Funktion "Date.add" 10 Tage hinzuzufügen. Das endgültige Datum wird dann formatiert und ausgegeben.

Hier ist eine Liste weiterer nützlicher Funktionen für Datumsberechnungen in Elm:

- `Date.subtract`: Erlaubt das Subtrahieren von Tagen, Monaten, Jahren und anderen Einheiten von einem Datum.

- `Date.dayOfWeek`: Gibt den Wochentag eines Datums zurück (von Montag bis Sonntag).

- `Date.day`: Gibt den Tag des Monats eines Datums zurück.

- `Date.month`: Gibt den Monat eines Datums zurück (von 1 für Januar bis 12 für Dezember).

- `Date.year`: Gibt das Jahr eines Datums zurück.

- `Date.fromString`: Erlaubt das Erstellen eines Datums aus einem String in einem bestimmten Format.

## Tief tauchen

Hinter den Kulissen verwendet Elm das Unix-Epoch-Format, um Daten zu speichern und Berechnungen durchzuführen. Die Funktionen der Module "Time" und "Date" abstrahieren jedoch diese Komplexität und bieten eine benutzerfreundlichere Erfahrung.

Es ist wichtig zu beachten, dass alle Datumsangaben in Elm im UTC-Format gespeichert werden. Dies bedeutet, dass Datumswerte je nach Zeitzone berechnet werden können. Um dies zu vermeiden, sollte man die Zeitzone explizit angeben, indem man den String "Z" (für UTC) oder eine bestimmte Zeitzone als Parameter verwendet.

Sie können auch benutzerdefinierte Datumsformate erstellen, indem Sie die Funktion `Date.fromString` verwenden und ein spezifisches Format in Klammern angeben. Informationen über die unterstützten Formate können in der Dokumentation von Elm gefunden werden.

## Siehe auch

- [Dokumentation von Time and Date in Elm](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Unix-Epoch auf Wikipedia](https://de.wikipedia.org/wiki/Unixzeit)
- [Elm Sprachhandbuch zu Time](https://guide.elm-lang.org/appendix/time.html)