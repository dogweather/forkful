---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:32.801096-07:00
description: "Das Parsen eines Datums aus einem String ist eine g\xE4ngige Aufgabe,\
  \ wenn es um die Verarbeitung von Benutzereingaben oder das Lesen von Daten aus\
  \ Dateien\u2026"
lastmod: '2024-03-13T22:44:53.682043-06:00'
model: gpt-4-0125-preview
summary: "Das Parsen eines Datums aus einem String ist eine g\xE4ngige Aufgabe, wenn\
  \ es um die Verarbeitung von Benutzereingaben oder das Lesen von Daten aus Dateien\u2026"
title: Einen Datum aus einem String analysieren
weight: 30
---

## Was & Warum?

Das Parsen eines Datums aus einem String ist eine gängige Aufgabe, wenn es um die Verarbeitung von Benutzereingaben oder das Lesen von Daten aus Dateien geht. Dabei wird die String-Daten in ein vom Programmiersystem erkanntes Datumsformat umgewandelt. In Rust ist dies essenziell für Operationen mit Daten, wie Vergleichen, Arithmetik oder Formatierung, und es verbessert die Datenvalidierung und -integrität in Anwendungen.

## Wie geht das:

### Verwendung der Standardbibliothek von Rust (`chrono` Crate)
Die Standardbibliothek von Rust enthält keine direkte Datumsverarbeitung, aber das weit verbreitete `chrono` Crate ist eine robuste Lösung für die Manipulation von Datum und Uhrzeit. Zuerst fügen Sie `chrono` zu Ihrer `Cargo.toml` hinzu:

```toml
[dependencies]
chrono = "0.4"
```

Verwenden Sie dann `chrono`, um einen Datumsstring in ein `NaiveDate`-Objekt zu parsen:

```rust
extern crate chrono;
use chrono::NaiveDate;

fn main() {
    let date_str = "2023-04-01";
    let date = NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
        .expect("Fehler beim Parsen des Datums");

    println!("Geparstes Datum: {}", date);
}

// Beispiel Ausgabe:
// Geparstes Datum: 2023-04-01
```

### Verwendung von Rusts fortgeschrittener Datum-Zeit-Verarbeitung (`time` Crate)
Für eine fortgeschrittenere Datum-Zeit-Verarbeitung, einschließlich einer ergonomischeren Verarbeitung, sollten Sie das `time` Crate in Betracht ziehen. Zuerst nehmen Sie es in Ihre `Cargo.toml` auf:

```toml
[dependencies]
time = "0.3"
```

Parsen Sie dann einen Datumsstring unter Verwendung des `Date`-Typs und `PrimitiveDateTime`:

```rust
use time::{Date, PrimitiveDateTime, macros::datetime};

fn main() {
    let date_str = "2023-04-01 12:34:56";
    let parsed_date = PrimitiveDateTime::parse(
        date_str, 
        &datetime!("%Y-%m-%d %H:%M:%S")
    ).expect("Fehler beim Parsen von Datum und Uhrzeit");

    println!("Geparstes Datum-Uhrzeit: {}", parsed_date);
}

// Beispiel Ausgabe:
// Geparstes Datum-Uhrzeit: 2023-04-01 12:34:56
```

Beide Beispiele zeigen, wie Rust mit Hilfe von Drittanbieter-Crates das Parsen von Datumsstrings in manipulierbare Datum-Objekte erleichtert, was es zu einem mächtigen Werkzeug für die Softwareentwicklung mit zeitbezogenen Daten macht.
