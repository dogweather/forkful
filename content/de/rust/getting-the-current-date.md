---
title:                "Rust: Das aktuelle Datum erhalten"
simple_title:         "Das aktuelle Datum erhalten"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Warum

Das Abrufen des aktuellen Datums ist für viele Programmierer eine alltägliche Aufgabe. Es kann hilfreich sein, um z.B. zeitgesteuerte Anwendungen zu erstellen oder das Aktualisierungsdatum von Dateien zu speichern. Aber warum sollten wir uns das in Rust ansehen?

## Wie man das aktuelle Datum in Rust abruft

Das Abrufen des aktuellen Datums in Rust ist einfach und erfordert nur wenige Zeilen Code. Zunächst müssen wir die Zeitbibliothek von Rust importieren, indem wir folgenden Code am Anfang unseres Programms hinzufügen:

```Rust
use std::time::SystemTime;
```

Als nächstes können wir die Funktion `now()` von `SystemTime` verwenden, um das aktuelle Datum und die Uhrzeit abzurufen:

```Rust
fn main() {
    let now = SystemTime::now();
}
```

Um das Datum in einem bestimmten Format auszugeben, können wir die Funktion `strftime()` verwenden. Diese Funktion nimmt eine Formatzeichenkette als Parameter und gibt das Datum entsprechend formatiert aus. Sehen wir uns ein Beispiel an, in dem wir das aktuelle Datum im ISO 8601-Format abrufen und ausgeben:

```Rust
use std::time::SystemTime;
use std::time::Duration;

fn main() {
    let now = SystemTime::now();
    let unix_timestamp = now.duration_since(SystemTime::UNIX_EPOCH).expect("Fehler beim Abrufen des UNIX-Zeitstempels.");
    
    let formatted_date = unix_timestamp.strftime("%Y-%m-%d").unwrap();
    println!("{}", formatted_date);
}
```

Die Ausgabe dieses Codes wäre zum Beispiel `2021-09-30`.

## Tiefergehende Informationen

In diesem Beispiel haben wir gesehen, wie man das aktuelle Datum mit Rust abrufen und formatieren kann. Es gibt jedoch noch viele weitere nützliche Funktionen in der `time`-Bibliothek, wie z.B. die Möglichkeit, die Uhrzeit und das Datum separat abzurufen oder die Zeitzone zu berücksichtigen. Für weitere Informationen zu diesen Funktionen und ihrer Verwendung schaut euch die offizielle Dokumentation an:

[https://doc.rust-lang.org/std/time/struct.SystemTime.html](https://doc.rust-lang.org/std/time/struct.SystemTime.html)

## Siehe auch

- [https://doc.rust-lang.org/std/time/struct.SystemTime.html](https://doc.rust-lang.org/std/time/struct.SystemTime.html) - Offizielle Dokumentation zu `SystemTime`
- [https://crates.io/crates/time](https://crates.io/crates/time) - `time`-Crate auf crates.io
- [https://github.com/rust-lang/rust/blob/master/src/libstd/time.rs](https://github.com/rust-lang/rust/blob/master/src/libstd/time.rs) - Quellcode der `time`-Bibliothek auf GitHub