---
title:                "Datum in der Zukunft oder Vergangenheit berechnen"
html_title:           "Elm: Datum in der Zukunft oder Vergangenheit berechnen"
simple_title:         "Datum in der Zukunft oder Vergangenheit berechnen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum jemand ein Datum in der Zukunft oder Vergangenheit berechnen möchte. Vielleicht möchten Sie wissen, an welchem Tag Ihr Geburtstag in ein paar Jahren sein wird, oder Sie müssen eine Frist für ein Projekt festlegen. Egal aus welchem Grund, Elm bietet eine einfache Möglichkeit, diese Berechnungen durchzuführen.

## How To

Um ein Datum in Elm zu berechnen, verwenden wir die `Time` Bibliothek. Zunächst müssen wir sie importieren:

```Elm
import Time
```

Dann können wir die Funktion `add` verwenden, um ein Datum in der Zukunft oder Vergangenheit zu berechnen. Diese Funktion erwartet zwei Argumente: die Anzahl der Einheiten, die Sie hinzufügen möchten, und die Zeit Einheit selbst. Zum Beispiel, wenn wir 10 Tage in die Zukunft berechnen möchten, würden wir Folgendes tun:

```Elm
Time.add 10 Time.day
```

Dies gibt uns ein `Posix` Datum zurück, das wir in unserer Anwendung verwenden können. Wenn wir das Datum im Format "Monat/Tag/Jahr" ausgeben möchten, können wir die `Time.Format` Bibliothek importieren und die `format` Funktion verwenden:

```Elm
import Time.Format

let
    dateInFuture = Time.add 10 Time.day
in
    Time.Format.format "%m/%d/%Y" dateInFuture
```

Dies würde uns ein Format wie "05/01/2021" zurückgeben, abhängig von dem aktuellen Datum.

## Deep Dive

Es ist wichtig zu verstehen, dass die `Time` Bibliothek in Elm auf `POSIX` basiert, was ein einheitlicher Zeit- und Datumsstandard ist. Dies bedeutet, dass wir immer mit `POSIX` Datumswerten arbeiten, die wir dann formatieren oder in unserer Anwendung verwenden können.

Eine weitere wichtige Funktion in der `Time` Bibliothek ist `now`, die uns das aktuelle Datum und die aktuelle Uhrzeit gibt. Wenn wir also ein Datum in der Zukunft oder Vergangenheit berechnen möchten, können wir dies basierend auf dem aktuellen Datum und der aktuellen Uhrzeit tun, indem wir `now` verwenden.

Ein weiteres zu beachtendes Detail ist, dass die `Time` Bibliothek nur für gregorianische Kalender unterstützt, was bedeutet, dass sie keine Unterstützung für andere Kalender wie den islamischen oder jüdischen Kalender bietet.

## Siehe auch

- Offizielle Elm `Time` Dokumentation: https://package.elm-lang.org/packages/elm/time/latest/
- Einführung in die Elm Programmierung: https://elm-lang.org/docs/from-javascript