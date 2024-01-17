---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Ruby: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Berechnen eines Datums in der Zukunft oder Vergangenheit ist eine nützliche Fähigkeit für Programmierer, die es ermöglicht, Datumsmanipulationen in ihren Anwendungen durchzuführen. Indem sie ein Datum in der Zukunft oder Vergangenheit berechnen, können Programmierer beispielsweise Termine für Aufgaben oder Erinnerungen automatisch erstellen oder Daten auf bestimmte Zeiträume beschränken.

## Wie geht das?
```Ruby
require 'date'

# Ein Datum in der Zukunft berechnen
Date.today + 7

# Ein Datum in der Vergangenheit berechnen
Date.today - 30
```

Das obige Beispiel verwendet die in Ruby integrierte `Date`-Klasse und die Methoden `today`, `+` und `-` um ein Datum in der Zukunft oder Vergangenheit zu berechnen. Durch die Verwendung von `require 'date'` wird die `Date`-Klasse in das laufende Programm geladen, falls dies nicht bereits geschehen ist. Die Methode `today` gibt das aktuelle Datum zurück, während `+` und `-` es erlauben, das Datum um eine bestimmte Anzahl von Tagen zu erweitern oder zu reduzieren.

Die Ausgabe des obigen Codes würde z.B. "2021-07-14" für das Datum in der Zukunft (eine Woche nach dem heutigen Datum) und "2021-06-14" für das Datum in der Vergangenheit (30 Tage vor dem heutigen Datum) sein.

## Tiefergehende Info
Das Berechnen von Datumsangaben ist eine nützliche Funktion für Programmierer, die seit langem existiert. Frühere Programmiersprachen wie Fortran und COBOL enthielten bereits Funktionen, die es ermöglichten, Datumsberechnungen durchzuführen. In Ruby sind weitere Möglichkeiten zur Manipulation von Datumswerten verfügbar, beispielsweise durch die Nutzung von Unix-Zeitstempeln oder durch die Verwendung von Bibliotheken wie `Chronic` und `Datejs`.

## Siehe auch
- [Ruby-Dokumentation zur Date-Klasse](https://ruby-doc.org/stdlib/libdoc/date/rdoc/Date.html)
- [Chronic-Ruby-Gem](https://github.com/mojombo/chronic)
- [Datejs-Bibliothek](https://datejs.com/)