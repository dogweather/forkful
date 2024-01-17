---
title:                "Das aktuelle Datum erhalten"
html_title:           "Rust: Das aktuelle Datum erhalten"
simple_title:         "Das aktuelle Datum erhalten"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die aktuelle Datumserfassung ist ein Standardverfahren in der Programmierung, bei dem das aktuelle Datum und die Uhrzeit aufgenommen werden. Dies dient dazu, wichtige Informationen über den Zeitpunkt der Ausführung von Programmen zu erhalten und sie gegebenenfalls zu überprüfen oder zu organisieren.

## Wie geht's:
```Rust
// Beispiele für die Ermittlung des aktuellen Datums
use chrono::{Local, DateTime};

// Aktuelles Datum und Uhrzeit abrufen
let now: DateTime<Local> = Local::now();
println!("Das aktuelle Datum ist: {}", now);

// Nur das aktuelle Datum abrufen
let date: Local = Local::today();
println!("Das aktuelle Datum ist: {}", date);
```
### Ausgabe:
Das aktuelle Datum ist: 2021-08-17 09:00:00+00:00
Das aktuelle Datum ist: 2021-08-17

## Tiefsee-Tauchgang:
Die Notwendigkeit, das aktuelle Datum in der Programmierung zu erfassen, geht zurück auf die Anfänge der Computer und die Entwicklung von Betriebssystemen. In früheren Systemen war es üblich, dass Programmierer manuell das Datum und die Uhrzeit überprüfen und eingeben mussten. Mit der Zeit wurden jedoch standardisierte Methoden zur automatischen Erfassung entwickelt, wie z.B. die heute weit verbreitete Chrono-Bibliothek in der Programmiersprache Rust.

Eine alternative Möglichkeit, das aktuelle Datum zu erfassen, ist die Nutzung von externen APIs oder Diensten, die speziell dafür entwickelt wurden, die aktuelle Zeit bereitzustellen. Dies kann je nach Anwendungsfall nützlich sein, da diese Dienste oft über Funktionen verfügen, die es ermöglichen, die Zeitzone oder das Format des Datums anzupassen.

In der Implementierung nutzt die Chrono-Bibliothek das Konzept der "system clock", das die vom Betriebssystem bereitgestellte Zeit nutzt. Je nach Betriebssystem kann dies unterschiedlich implementiert sein, aber im Allgemeinen ist es eine zuverlässige und genaue Methode, um die aktuelle Datum und Uhrzeit zu erfassen.

## Siehe auch:
- [The Rust Programming Language](https://www.rust-lang.org) - Offizielle Website der Programmiersprache Rust
- [Chrono Crate Documentation](https://docs.rs/chrono) - Offizielle Dokumentation der Chrono-Bibliothek in Rust
- [The History of Date & Time](https://blog.codinghorror.com/the-history-of-date-and-time) - Artikel über die Geschichte und Entwicklung der Datums- und Zeitfunktionen in der Programmierung