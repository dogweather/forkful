---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Gleam: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Es kann viele Gründe geben, warum man sich mit der Berechnung von zukünftigen oder vergangenen Daten beschäftigen möchte. Vielleicht planst du eine Reise und möchtest wissen, wie viele Tage es noch bis dahin sind. Oder du entwickelst eine Software, die Geburtstage oder Vertragslaufzeiten verwalten muss. Egal aus welchem Grund, die Fähigkeit, Datumsberechnungen durchzuführen, ist für viele Anwendungsfälle unerlässlich.

## Wie geht das?

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, musst du zunächst das `DateTime` Modul in Gleam importieren. Dann kannst du die Funktion `add` verwenden, um die gewünschte Anzahl von Tagen, Monaten oder Jahren zu einem Datum hinzuzufügen oder davon abzuziehen.

```Gleam
import gleam/datetime
let twenty_first_october_2021 = datetime.DateTime(
    year = 2021,
    month = October,
    day = 21,
    hour = 12,
    minute = 0,
    second = 0,
    nano = 0,
)

let three_days_later = datetime.add(
    date = twenty_first_october_2021,
    duration = datetime.Duration(days = 3)
)

datetime.to_string(twenty_first_october_2021) // "2021-10-21T12:00:00Z"
datetime.to_string(three_days_later) // "2021-10-24T12:00:00Z"
```

## Tiefer in die Materie

Die `DateTime`- und `Duration`-Module in Gleam sind auf dem Niveau von Datums- und Zeitberechnungen sehr leistungsfähig. Du kannst nicht nur Tage, Monate und Jahre hinzufügen oder abziehen, sondern auch beliebige Zeitspannen, einschließlich Nanosekunden. Die Gleam-Compiler bietet auch integrierte Funktionen zur Unterstützung der Umstellung von Zeitzonen. Überprüfe die offizielle Dokumentation für weitere Details.

## Siehe auch

- Offizielle Gleam-Dokumentation für das `DateTime`-Modul: https://gleam.run/documentation/standard-library#datetime
- Gleam-Beispielcode für Datumsberechnungen: https://github.com/gleam-lang/gleam/blob/master/examples/datetime.gleam
- Andere nützliche Funktionen der Gleam-Standardbibliothek: https://gleam.run/documentation/standard-library