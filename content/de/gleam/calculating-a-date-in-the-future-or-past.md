---
title:                "Berechnung eines zukünftigen oder vergangenen Datums"
html_title:           "Gleam: Berechnung eines zukünftigen oder vergangenen Datums"
simple_title:         "Berechnung eines zukünftigen oder vergangenen Datums"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?

Berechnungen von Daten in der Zukunft oder Vergangenheit sind Methoden, um ein bestimmtes Datum basierend auf einem anderen zu ermitteln. Programmierer machen das, weil Zeit wichtige Rollen in Softwareanwendungen spielt, von Erinnerungsfunktionen bis hin zu Ablaufdaten.

## Wie geht's:

Hier ist ein einfaches Beispiel dafür, wie man das in Gleam (der aktuellen Version) macht.

```Gleam
import gleam/date
import gleam/time.{next_day, previous_day}

// Ein Beispiel für die Berechnung eines zukünftigen Datums
let tomorrow = next_day(date.new(2021, 12, 31))

//Ein Beispiel für die Berechnung eines vergangenen Datums
let yesterday = previous_day(date.new(2021, 12, 31))

```
Die Ausgabe von 'tomorrow' wäre `2022-01-01` und die von 'yesterday' wäre `2021-12-30`.

## Tiefere Einblicke:

Historisch gesehen wurde die Berechnung von Datumsangaben bereits in der Frühe der Computertechnik eingesetzt, meistens zum Verwalten von Zeitstempeln und Terminen. Mit der fortschreitenden Entwicklung hat sich das weitaus komplizierter und nuancierter gestaltet. Alternativen zu Gleam können andere Programmiersprachen sein wie Python, Java oder JavaScript, aber Gleam bietet eine saubere und kompakte Art, Datum zu berechnen. 

Eines der Dinge, die bei der Datumsberechnung zu beachten sind, ist die Berücksichtigung von Schaltjahren.

## Siehe auch:

Prüfen diese Ressourcen aus, um weiter zu lernen:

- Gleam-Dokumentation: [https://gleam.run/book/tour/dates-and-times.html](https://gleam.run/book/tour/dates-and-times.html)
- Allgemeine Datums- und Zeitfunktionen: [https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Über die Complexity of Time Data Types und Time Arithmetic: [https://infolab.stanford.edu/~ullman/fcdb/material/handouts/time.pdf](https://infolab.stanford.edu/~ullman/fcdb/material/handouts/time.pdf)