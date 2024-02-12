---
title:                "Berechnung eines zukünftigen oder vergangenen Datums"
aliases:
- de/rust/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:54.270225-07:00
model:                 gpt-4-1106-preview
simple_title:         "Berechnung eines zukünftigen oder vergangenen Datums"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Berechnen eines Datums in der Zukunft oder Vergangenheit bedeutet, zu einem gegebenen Datum eine bestimmte Zeitdauer hinzuzufügen oder abzuziehen. Programmierer nutzen das für Features wie Erinnerungen, Ablaufdaten oder Zeitplanungen.

## Anleitung:
```Rust
use chrono::{Duration, Utc};

fn main() {
    let heute = Utc::now();
    println!("Heute: {}", heute.format("%d.%m.%Y"));

    let zukunft = heute + Duration::days(30);
    println!("In 30 Tagen: {}", zukunft.format("%d.%m.%Y"));

    let vergangenheit = heute - Duration::weeks(4);
    println!("Vor 4 Wochen: {}", vergangenheit.format("%d.%m.%Y"));
}
```
Ausgabe:
```
Heute: 09.04.2023
In 30 Tagen: 09.05.2023
Vor 4 Wochen: 12.03.2023
```

## Hintergrund:
Früher nutzte man einfache Arithmetik und berücksichtigte manuell Schaltjahre und Monatslängen beim Berechnen von Daten. In Rust gibt's dank Bibliotheken wie `chrono` bequeme Funktionen dafür. Alternativen wie die Standardbibliothek `std::time` oder `time` bieten ähnliche Funktionalitäten. `chrono` ist sehr verbreitet und hat umfangreiche Typen und Methoden für Zeitberechnungen. Man kann mit `Duration` Objekten Zeiträume addieren oder subtrahieren und muss sich nicht um die Details der Datumsarithmetik kümmern.

## Siehe Auch:
- [Chrono Crate Documentation](https://docs.rs/chrono/)
- [Rust Standard Library Time Module](https://doc.rust-lang.org/std/time/)
- [Time Crate Documentation](https://docs.rs/time/)
