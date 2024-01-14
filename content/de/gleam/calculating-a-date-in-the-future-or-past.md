---
title:                "Gleam: Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Manchmal müssen wir in unseren Programmen zukünftige oder vergangene Termine berechnen. Zum Beispiel möchten wir vielleicht ein Geburtstagsdatum für die nächsten 10 Jahre berechnen oder ein Abonnementdatum in der Vergangenheit überprüfen. In solchen Fällen ist es nützlich zu wissen, wie man Datumsberechnungen in Gleam durchführt.

## Wie geht man vor

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, müssen wir die `gleam/time`-Bibliothek importieren und die Funktion `add_time` oder `sub_time` verwenden. Lassen Sie uns zunächst ein Beispiel machen, um das aktuelle Datum zu erhalten:

```Gleam
import gleam/time
today = time.now() // Heutiges Datum
```

Um nun ein zukünftiges Datum zu berechnen, können wir die `add_time`-Funktion verwenden und die Anzahl der Tage, Monate oder Jahre, die wir hinzufügen möchten, als Argument übergeben. Es handelt sich dabei um einen `Time.Duration`-Wert, der aus der `gleam/core`-Bibliothek importiert werden muss.

Lassen Sie uns ein Beispiel machen, um das Datum in 10 Monaten zu berechnen:

```Gleam
import gleam/time
import gleam/core
today = time.now()
nextDate = time.add_time(today, core.Duration(months: 10))
// Datum in 10 Monaten
```

Um ein vergangenes Datum zu berechnen, können wir die `sub_time`-Funktion verwenden und die Dauer, die wir abziehen möchten, als Argument übergeben. Hier ist ein Beispiel, um das Datum vor 2 Wochen zu berechnen:

```Gleam
import gleam/time
import gleam/core
today = time.now()
pastDate = time.sub_time(today, core.Duration(weeks: 2))
// Datum vor 2 Wochen
```

## Tiefere Einblicke

Möglicherweise möchten Sie auch Zeitangaben wie Stunden, Minuten oder Sekunden berücksichtigen, um ein genaueres Datum zu berechnen. Dafür können wir die `Time.Duration`-Datentypen `hours`, `minutes` und `seconds` verwenden. Schauen wir uns ein Beispiel an, um das Datum genau 6 Stunden und 30 Minuten in der Zukunft zu berechnen:

```Gleam
import gleam/time
import gleam/core
today = time.now()
nextDate = time.add_time(today, core.Duration(hours: 6, minutes: 30))
// Datum in 6 Stunden und 30 Minuten
```

Es gibt auch andere nützliche Funktionen in der `gleam/time`-Bibliothek, wie zum Beispiel `compare_time`, um zwei Datumswerte zu vergleichen, oder `to_string` um ein Datum in einem bestimmten Format auszugeben.

## Siehe auch

- [Gleam Dokumentation zu Datum und Zeit](https://gleam.run/modules/time)
- [Gleam Zeit Funktionen im Core Modul](https://gleam.run/modules/core#Time.Duration)
- [Verfügbarer Datentyp in der Glean Standardbibliothek](https://github.com/gleam-lang/gleam_stdlib/blob/master/std/time/time.gleam)