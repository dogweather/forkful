---
title:                "Datum aus einem String parsen"
date:                  2024-01-20T15:38:35.478403-07:00
simple_title:         "Datum aus einem String parsen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String verwandelt Text in ein Datum-Objekt. Das brauchen Programmierer, um mit Datumsangaben leichter rechnen und sie in verschiedenen Formaten bearbeiten zu können.

## How to:
```Rust
use chrono::{DateTime, NaiveDateTime, Utc, Local, TimeZone};

fn main() {
    let date_string = "2023-04-02T17:00:00Z";
    let parsed_date = DateTime::parse_from_rfc3339(date_string).unwrap();

    println!("{}", parsed_date); // 2023-04-02 17:00:00 UTC

    // Falls man keine Zeitzone hat:
    let naive_date_string = "2023-04-02T17:00:00";
    let naive_parsed_date = NaiveDateTime::parse_from_str(naive_date_string, "%Y-%m-%dT%H:%M:%S").unwrap();

    // In lokale Zeit umwandeln:
    let local_date = Local.from_utc_datetime(&naive_parsed_date);
    
    println!("{}", local_date); // Lokale Zeit, z.B.: 2023-04-02 19:00:00 +02:00
}
```
## Deep Dive
Früher griffen Rust-Entwickler oft zur `time`-Crate für Zeitoperationen, aber mit der `chrono`-Crate haben wir nun eine mächtigere Bibliothek. `chrono` unterstützt verschiedene Kalender und ist auch für komplexe Aufgaben wie Zeitberechnungen oder Formatwandlungen besser geeignet. Das obige Beispiel nutzt `chrono` für Parsing-Aufgaben. Für `NaiveDateTime` ist kein Bewusstsein der Zeitzone notwendig, was für bestimmte Anwendungen nützlich sein kann. Andererseits, wenn 'Time Zone'-Informationen relevant sind, kommt `DateTime` ins Spiel.

Eine beachtenswerte Alternative ist die Standardbibliotheks-Funktion `strptime()`, die direkt mit formatierten Strings arbeitet. `chrono` allerdings bietet strengere Fehlerprüfungen und ist flexibler.

Die Implementierungsdetails sind wichtig, weil falsches Parsen zu Fehlern führen kann - denk an Schaltsekunden oder Zeitzone-Anomalien. Ein solides Verständnis von `chrono` und dessen Parsing-Funktionen kann viele Kopfschmerzen ersparen.

## See Also
- Die `chrono`-Crate-Dokumentation: https://docs.rs/chrono/0.4.19/chrono/
- Rusts Modul für Systemzeit (wenn keine `chrono` erforderlich): https://doc.rust-lang.org/std/time/index.html
- Rust-Date-Handling-Diskussion auf users.rust-lang.org: https://users.rust-lang.org/t/how-to-deal-with-dates-in-rust/2928
