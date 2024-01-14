---
title:    "Elm: Berechnung eines Datums in der Zukunft oder Vergangenheit"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Datumsangaben in der Zukunft oder Vergangenheit kann in verschiedenen Anwendungen sehr nützlich sein. Zum Beispiel kann dies bei der Planung von Terminen oder der Erstellung von Kalendern hilfreich sein. Mit Elm können wir diese Aufgabe effizient und präzise angehen.

## Wie

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, können wir die `add`-Funktion aus dem `Calendar`-Modul von Elm verwenden. Diese Funktion akzeptiert ein Datum und die Anzahl der zu addierenden Tage und gibt das resultierende Datum zurück. Hier ist ein Beispiel, um das Datum von 2 Tagen in der Zukunft zu berechnen:

```Elm
Calendar.add (Calendar.days 2) model.selectedDate
```

In diesem Beispiel verwenden wir die Funktion `days` aus dem `Calendar`-Modul, um eine Anzahl von Tagen (hier 2) zu erstellen und dieses dann mit der `add`-Funktion zu kombinieren. Das `model.selectedDate` ist das Startdatum, auf das wir die Anzahl der Tage addieren möchten. Die `add`-Funktion gibt ein neues Datum zurück, das 2 Tage in der Zukunft liegt.

Um ein Datum in der Vergangenheit zu berechnen, können wir die `sub`-Funktion verwenden, die ähnlich wie `add` funktioniert, aber die Tage von dem angegebenen Datum subtrahiert.

```Elm
Calendar.sub (Calendar.days 10) model.selectedDate
```

Dieses Beispiel gibt das Datum 10 Tage vor dem `model.selectedDate` aus.

## Deep Dive

Um das Berechnen von Datumsangaben in der Zukunft oder Vergangenheit noch genauer zu verstehen, ist es wichtig, die internen Datenstrukturen von Elm zu verstehen. In Elm gibt es ein `Date`-Modul, das eine benutzerfreundliche Schnittstelle für die Verwendung von Datumswerten bietet. Aber intern verwendet dieses Modul `Posix`-Werte, die den Abstand eines Datums von dem 1. Januar 1970 in Millisekunden darstellen. Wenn wir also ein Datum in die Zukunft oder Vergangenheit addieren oder subtrahieren, manipulieren wir diese Posix-Werte.

## Siehe auch

- Offizielle Elm Dokumentation zu `Calendar`: https://package.elm-lang.org/packages/elm/time/latest/Calendar
- Grundlagen der Datum- und Zeitberechnung in Elm: https://guide.elm-lang.org/architecture/effects/time.html#date-and-time
- Beispielprojekt für die Berechnung von Daten in der Zukunft oder Vergangenheit: https://github.com/elm/projects/tree/master/DateCalculation