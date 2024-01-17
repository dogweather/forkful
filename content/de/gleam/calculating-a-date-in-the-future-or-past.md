---
title:                "Ein Datum in der Zukunft oder Vergangenheit berechnen"
html_title:           "Gleam: Ein Datum in der Zukunft oder Vergangenheit berechnen"
simple_title:         "Ein Datum in der Zukunft oder Vergangenheit berechnen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Was & Warum? 
Berechnung einer zukünftigen oder vergangenen Datum ist ein wichtiger Teil der Programmierung, der es uns ermöglicht, bestimmte Aktionen basierend auf einem bestimmten Datum auszuführen. Programmierer verwenden diese Funktion, um beispielsweise Termine für geplante Aufgaben oder Ereignisse zu setzen.

Wie geht's?
Hier sind zwei Beispiele, wie du mit Gleam ein Datum in der Zukunft oder Vergangenheit berechnen kannst:

```Gleam
// Ein Tag in der Zukunft
let zukunft = Date.add_days(Date.utc_today(), 1)

// Fünf Jahre in der Vergangenheit
let vergangenheit = Date.add_years(Date.utc_today(), -5)

// Ausgabe: zukunft = 2021-05-05 und vergangenheit = 2016-05-05
```

Tiefer Einblick
Die Idee, ein Datum in der Zukunft oder Vergangenheit zu berechnen, ist nicht neu und wurde schon in frühen Programmiersprachen wie COBOL implementiert. Gleam bietet jedoch eine benutzerfreundliche Syntax und eingebaute Funktionen, die es uns ermöglichen, auf einfache Weise beliebige Zeiteinheiten zu berechnen.

Alternativ gibt es auch externe Bibliotheken, wie z.B. die Java Time Library, die ähnliche Funktionen bereitstellen. Allerdings ist Gleam eine kompilierte Sprache, was bedeutet, dass alle Fehler während der Kompilierung erkannt werden, während bei interpretierten Sprachen wie Java die Fehler erst während der Ausführung bemerkt werden.

Weiterführende Links
- Gleam-Referenzhandbuch: https://gleam.run/book/core_types/date.html
- Java Time Library: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html