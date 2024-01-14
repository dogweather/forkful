---
title:    "Rust: Vergleich von zwei Daten"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum
In dieser Blog-Post geht es um die vergleichende Datumsfunktion in der Programmiersprache Rust. Warum sollte man das überhaupt tun? Vergleichen von Daten kann sehr hilfreich sein, um zum Beispiel zu überprüfen, ob ein Datum in der Zukunft oder Vergangenheit liegt oder um komplexe Zeitberechnungen durchzuführen.

## Anleitung
Um zwei Daten in Rust zu vergleichen, gibt es verschiedene Möglichkeiten. Eine einfache Möglichkeit ist die Verwendung des `date`-Pakets, das nützliche Funktionen für die Arbeit mit Datumswerten bietet.

```Rust
use date::{Date, Locale};

fn main() {
    let date1 = Date::parse("2021-01-01", "y-M-d", Locale::en_US).unwrap();
    let date2 = Date::parse("2020-12-31", "y-M-d", Locale::en_US).unwrap();
    // Vergleich der Daten mit der Funktion `cmp`
    let comparison = date1.cmp(&date2);
    println!("{}", comparison); // Output: Greater
}
```

In diesem Beispiel werden zwei Datumswerte aus Strings erzeugt und dann miteinander verglichen. Die Funktion `cmp` gibt ein Enum zurück, das angibt, ob das erste Datum größer, kleiner oder gleich dem zweiten Datum ist.

Eine weitere Möglichkeit ist die Verwendung des `time`-Pakets, das es ermöglicht, komplexe Zeitberechnungen durchzuführen und Daten mit unterschiedlichen Zeitzonen zu vergleichen.

```Rust
use time::OffsetDateTime;

fn main() {
    // Datum mit Zeitzone erzeugen
    let date1 = OffsetDateTime::parse("2020-01-01T00:00:00+00:00", "%Y-%m-%dT%H:%M:%S%:z");
    let date2 = OffsetDateTime::parse("2020-01-01T00:00:00+01:00", "%Y-%m-%dT%H:%M:%S%:z");
    // Vergleich der Daten mit der Funktion `partial_cmp`
    let comparison = date1.partial_cmp(&date2);
    println!("{:?}", comparison); // Output: Some(Greater)
}
```

Hier wird zunächst ein Datum mit einer Zeitzone erzeugt und dann mit einem anderen Datum verglichen. Die Funktion `partial_cmp` gibt ein `Option<Ordering>` zurück, das angibt, ob das erste Datum größer, kleiner oder gleich dem zweiten Datum ist. Ein `Some` wird zurückgegeben, wenn der Vergleich erfolgreich war, ansonsten `None`.

## Tiefergehendes
Beim Vergleich von Daten gibt es einige wichtige Dinge zu beachten, wie zum Beispiel die Berücksichtigung von Zeitzone oder die Umwandlung von Daten in einen anderen Datentyp. Eine detaillierte Erklärung dazu würde den Rahmen dieses Blog-Posts sprengen, aber es ist wichtig zu wissen, dass es je nach Anwendungsfall verschiedene Ansätze gibt und es wichtig ist, die richtige Methode für den Vergleich zu wählen.

## Siehe auch
- [Offizielle Dokumentation des `date`-Pakets](https://crates.io/crates/date)
- [Offizielle Dokumentation des `time`-Pakets](https://crates.io/crates/time)
- [Beispielcode für die Arbeit mit Datumswerten in Rust](https://github.com/rust-lang/chrono/tree/master/examples)