---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:47.604989-07:00
description: "Das Abrufen des aktuellen Datums in Rust ist eine g\xE4ngige Aufgabe\
  \ f\xFCr Dinge wie das Logging, zeitbasierte Operationen oder einfach nur, um das\
  \ Datum\u2026"
lastmod: '2024-02-25T18:49:50.750909-07:00'
model: gpt-4-0125-preview
summary: "Das Abrufen des aktuellen Datums in Rust ist eine g\xE4ngige Aufgabe f\xFC\
  r Dinge wie das Logging, zeitbasierte Operationen oder einfach nur, um das Datum\u2026"
title: Den aktuellen Datum abrufen
---

{{< edit_this_page >}}

## Was & Warum?

Das Abrufen des aktuellen Datums in Rust ist eine gängige Aufgabe für Dinge wie das Logging, zeitbasierte Operationen oder einfach nur, um das Datum anzuzeigen. Im Gegensatz zu einigen Sprachen, die Datum- und Zeitfunktionalitäten in ihrer Standardbibliothek enthalten, ermutigt Rust zur Nutzung einer robusten Drittanbieterbibliothek, chrono, für umfassende Datum- und Zeitmanipulationen aufgrund ihrer überlegenen Funktionalität und Benutzerfreundlichkeit.

## Wie geht das:

### Verwendung von Rusts Standardbibliothek
Rusts Standardbibliothek bietet eine begrenzte, aber schnelle Möglichkeit, die aktuelle Zeit zu ermitteln, allerdings nicht direkt das aktuelle Datum im Kalenderformat. So machen Sie es:

```rust
use std::time::{SystemTime, UNIX_EPOCH};

fn main() {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(n) => println!("Aktuelle Zeit: {} Sekunden seit der Unix-Epoche.", n.as_secs()),
        Err(_) => panic!("SystemTime vor der Unix-Epoche!"),
    }
}
```

Ausgabe:
```
Aktuelle Zeit: 1615390665 Sekunden seit der Unix-Epoche.
```

### Verwendung der Chrono-Bibliothek
Für umfassendere Datums- und Zeitfunktionalitäten, einschließlich des Abrufens des aktuellen Datums, sollten Sie die Bibliothek `chrono` verwenden. Fügen Sie zuerst `chrono` zu Ihrer `Cargo.toml` hinzu:

```toml
[dependencies]
chrono = "0.4"
```

Danach können Sie `chrono` verwenden, um das aktuelle Datum zu erhalten:

```rust
extern crate chrono;
use chrono::{Local, Datelike};

fn main() {
    let now = Local::now();
    println!("Aktuelles Datum: {}-{}-{}", now.year(), now.month(), now.day());
}
```

Ausgabe:
```
Aktuelles Datum: 2023-4-20
```

Die Bibliothek `chrono` macht es unkompliziert, mit Daten und Zeiten zu arbeiten, und bietet eine breite Palette von Funktionalitäten über das bloße Abrufen des aktuellen Datums hinaus, einschließlich des Parsens, Formatierens und der Durchführung von arithmetischen Operationen mit Daten und Zeiten.
