---
title:                "Rust: Die aktuelle Datum abrufen"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Warum

Das Abrufen des aktuellen Datums ist ein grundlegender Bestandteil jeder Programmiersprache. Es kann in vielen Anwendungsfällen nützlich sein, z.B. für die Verwendung in Zeitstempeln oder für die Berechnung von Zeitunterschieden.

## Wie

Im Folgenden finden Sie Beispiele, wie Sie mit Rust das aktuelle Datum abrufen können.

```Rust
use std::time::{SystemTime, UNIX_EPOCH};

// Datum und Uhrzeit im Unix-Timestamp Format abrufen
let current_time = SystemTime::now()
    .duration_since(UNIX_EPOCH)
    .expect("Time went backwards");

println!("Unix timestamp: {:?}", current_time.as_secs());

// Datum und Uhrzeit im ISO 8601 Format abrufen
let current_date = Utc::today();

println!("Today's date: {}", current_date);
```

Die obigen Beispiele nutzen die Bibliothek "chrono" und die Typen "SystemTime" und "Utc". Sie können an Ihre eigenen Bedürfnisse angepasst werden, indem Sie die Formatierung des Datums oder die Zeitzone ändern.

## Deep Dive

Die Datums- und Uhrzeitmanipulation in Rust ist durch die Bibliothek "chrono" sehr einfach und intuitiv. Sie bietet viele nützliche Funktionen, z.B. die Umwandlung von Datentypen oder die Berechnung von Zeitunterschieden.

Es gibt auch weitere Bibliotheken, die das Arbeiten mit Datums- und Uhrzeitfunktionen in Rust erleichtern, z.B. "chrono-tz" für die Verwendung von Zeitzonen oder "datetime" für die Manipulation von Datumsobjekten.

Insgesamt ist das Arbeiten mit Datums- und Uhrzeitfunktionen in Rust effizient und robust. Es gibt viele nützliche Bibliotheken und die Syntax ist leicht zu verstehen.

# Siehe auch

- [Rust Dokumentation über Datums- und Uhrzeitfunktionen](https://doc.rust-lang.org/std/time/)
- [Chrono Bibliothek](https://crates.io/crates/chrono)
- [Chrono-tz Bibliothek](https://crates.io/crates/chrono-tz)
- [Datetime Bibliothek](https://crates.io/crates/datetime)