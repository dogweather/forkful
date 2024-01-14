---
title:                "Gleam: Ein Datum in der Zukunft oder Vergangenheit berechnen"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen eines Datums in der Zukunft oder Vergangenheit kann für viele Programmiererinnen und Programmierer eine nützliche Fähigkeit sein. Sei es für die Planung von Terminen oder das Durchführen von Datenanalysen, die Fähigkeit, zukünftige oder vergangene Termine zu berechnen, kann viel Zeit und Aufwand sparen.

## How To

Um ein Datum in der Zukunft oder Vergangenheit in Gleam zu berechnen, verwenden wir die Funktion `Date.add_days()` aus der `Date`-Bibliothek. Nehmen wir zum Beispiel an, wir möchten das Datum in 10 Tagen in der Zukunft berechnen. Die Codebeispiele und Ausgabe sehen wie folgt aus:

```Gleam
import Date

let future_date = Date.add_days(Date.now(), 10)

// Ausgabe: 2021-08-10
```

Um ein Datum in der Vergangenheit zu berechnen, nutzen wir die Funktion `Date.sub_days()`, wie im folgenden Beispiel:

```Gleam
import Date

let past_date = Date.sub_days(Date.now(), 5)

// Ausgabe: 2021-07-26
```

## Deep Dive

Um ein tieferes Verständnis davon zu erlangen, wie Datumskalkulation in Gleam funktioniert, sollten wir uns die `Date`-Bibliothek genauer ansehen. Diese Bibliothek bietet neben den Funktionen `add_days()` und `sub_days()` auch weitere nützliche Funktionen wie `get_day()`, `get_month()` oder `get_year()`, mit denen wir spezifische Daten aus einem Datum extrahieren können. Außerdem können wir nicht nur mit Tagen, sondern auch mit Wochen, Monaten oder Jahren rechnen. Die `Date`-Bibliothek bietet auch Funktionen für zeitliche Differenzen und Vergleiche zwischen verschiedenen Datumsangaben.

## Siehe auch

- [Offizielle Gleam Dokumentation](https://gleam.run/documentation/standard-library/date)
- [Gleam Github Repository](https://github.com/gleam-lang/gleam)