---
title:                "Aktuelles Datum abrufen"
date:                  2024-01-20T15:16:23.293433-07:00
html_title:           "C: Aktuelles Datum abrufen"
simple_title:         "Aktuelles Datum abrufen"

category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums ist essentiell, um in Programmen zeitlich relevante Aktionen zu tätigen. Programmierer tun dies, um etwa Zeitstempel zu generieren, Nutzeraktivitäten zu loggen oder datumsabhängige Funktionen auszuführen.

## Anleitung:
Die Verwendung des `chrono` Crates ermöglicht ein problemloses Abrufen des aktuellen Datums.

```Rust
extern crate chrono; // Beginne mit dem Einfügen des Crates an den Anfang des Code
use chrono::{Local, Datelike}; // Lade die benötigten Traits

fn main() {
    let heute = Local::today(); // Hol das heutige Datum
    println!("Heutiges Datum: {}", heute.format("%Y-%m-%d")); // Gibt das Datum im Format JJJJ-MM-TT aus
}

// Mögliche Ausgabe:
// Heutiges Datum: 2023-04-05
```

## Vertiefung:
Historisch gesehen bot Rusts Standardbibliothek (`std`) keine ausgereiften Zeitfunktionen, deswegen ist der `chrono` Crate zur De-facto-Lösung geworden. Alternativen wie die `time` Crate oder die Nutzung der `SystemTime` Klasse aus `std` bieten unterschiedliche Abstraktionsebenen und Möglichkeiten. Beim Abrufen des Datums muss man auch Zeitzonen beachten – `Local::today()` greift auf die lokale Zeit des Computers zu.

## Siehe Auch:
- Rust `chrono` Crate Dokumentation: [https://docs.rs/chrono](https://docs.rs/chrono)
- Rust Zeit- und Datumsprimitiven in `std`: [https://doc.rust-lang.org/std/time/index.html](https://doc.rust-lang.org/std/time/index.html)
