---
title:                "Berechnung eines zukünftigen oder vergangenen Datums"
date:                  2024-01-20T17:30:51.301233-07:00
model:                 gpt-4-1106-preview
simple_title:         "Berechnung eines zukünftigen oder vergangenen Datums"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Berechnen eines Datums in der Zukunft oder Vergangenheit bedeutet, ausgehend von einem bekannten Datum ein neues Datum zu ermitteln, das eine bestimmte Zeitspanne entfernt liegt. Programmierer nutzen dies für Funktionen wie Erinnerungen, Terminplanungen oder Zeitvergleiche.

## How to:
Gleam bietet keine integrierte Zeitbibliothek. Aber du kannst `gleam_stdlib` nutzen, ähnlich zu dieser Art:

```gleam
import gleam/calendar.{ Date, Duration, add }

pub fn calculate_future_date(base_date: Date, days_to_add: Int) -> Date {
  let duration_to_add = Duration(days: days_to_add)
  add(base_date, duration_to_add)
}

pub fn main() {
  let today = Date(year: 2023, month: 4, day: 15)
  let tomorrow = calculate_future_date(today, 1)
  tomorrow
}
```

Sample output:

```gleam
Date(year: 2023, month: 4, day: 16)
```

## Deep Dive
Historisch lehnt sich Gleam an funktionale Sprachen wie Erlang und OCaml an, jedoch ohne eingebaute Zeitverwaltung. Alternativen Optionen sind Bibliotheken wie `chronos` oder Bindungen zu Erlangs `:calendar`. Beim Rechnen mit Daten ist es wichtig, Timezones und Schaltjahre zu berücksichtigen, was in Bibliotheken eingebaut sein kann.

## Siehe Auch
- [Erlang's calendar module documentation](http://erlang.org/doc/man/calendar.html)
- [Chronos Gleam library](https://hex.pm/packages/chronos)
