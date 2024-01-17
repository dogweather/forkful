---
title:                "Datum in der Zukunft oder Vergangenheit berechnen."
html_title:           "Go: Datum in der Zukunft oder Vergangenheit berechnen."
simple_title:         "Datum in der Zukunft oder Vergangenheit berechnen."
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Berechnen eines Datums in der Vergangenheit oder Zukunft ist ein häufiges Problem, dem sich viele Programmierer in ihrem Alltag stellen müssen. Es geht darum, ein bestimmtes Datum durch Hinzufügen oder Subtrahieren von Tagen, Monaten oder Jahren zu verändern. Programmierer tun dies, um zukünftige Termine zu planen oder vergangene Ereignisse zu analysieren.

## Wie geht es?
Um ein Datum in der Vergangenheit oder Zukunft zu berechnen, können wir die Funktionen AddDate oder SubDate der Zeitbibliothek in Go verwenden. Wir geben einfach das ursprüngliche Datum und die gewünschte Anzahl an Tagen, Monaten oder Jahren an, die hinzugefügt oder subtrahiert werden sollen.

```Go
import "time"

// Berechnung eines Datums in der Zukunft
future := time.Now().AddDate(0, 1, 0) // Hinzufügen eines Monats zum aktuellen Datum
fmt.Println(future) // Output: 2021-01-09 15:55:05.815745877 +0100 CET m=+266.361063175

// Berechnung eines Datums in der Vergangenheit
past := time.Now().SubDate(0, 0, 7) // Subtrahieren von 7 Tagen vom aktuellen Datum
fmt.Println(past) // Output: 2020-11-30 15:55:05.815825609 +0100 CET m=+199.361142907
```

## Tiefer schürfen
Das Problem der Berechnung von Datumsangaben ist schon lange bekannt und es gibt verschiedene Alternativen, dies zu bewältigen. Eine Möglichkeit ist die Verwendung von Unix-Zeitstempeln, die die Anzahl der vergangenen Sekunden seit dem 1. Januar 1970 darstellen. Eine andere Möglichkeit ist die Verwendung von externen APIs, die für die Berechnung von Datumsangaben entwickelt wurden.

In der Implementierung nutzt die Funktion AddDate oder SubDate die interne Struktur time.Time und ändert diese entsprechend der angegebenen Werte. Dies ermöglicht eine sehr effiziente Berechnung von Datumsangaben.

## Sieh auch
- [Go Zeitbibliothek](https://pkg.go.dev/time)
- [Unix-Zeitstempel](https://de.wikipedia.org/wiki/Unixzeit)