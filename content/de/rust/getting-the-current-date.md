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

## Warum

Das Holen des aktuellen Datums ist eine häufige Aufgabe in vielen Programmen. Zum Beispiel könnte es für ein Tagebuch- oder Kalenderprogramm wichtig sein, das aktuelle Datum für Einträge oder Termine zu erfassen. In diesem Artikel werden wir uns ansehen, wie man dies in der Programmiersprache Rust umsetzen kann.

## Wie es geht

Um das aktuelle Datum in Rust abzurufen, müssen wir zunächst die Standardbibliothek `std` importieren, die wichtige Funktionen und Datentypen zur Verfügung stellt.

```Rust
use std::time::SystemTime;
```

Als nächstes können wir die Methode `now()` der Struktur `SystemTime` aufrufen, um die aktuelle Zeit zu erhalten.

```Rust
let now = SystemTime::now();
```

Um das Datum aus dem `SystemTime`-Objekt zu extrahieren, können wir die Methode `duration_since()` mit dem Unix-Epoch (1. Januar 1970) als Argument aufrufen und das resultierende `Duration`-Objekt verwenden.

```Rust
use std::time::{Duration, UNIX_EPOCH};
let now = SystemTime::now();
let unix_epoch = UNIX_EPOCH;
let duration = now.duration_since(unix_epoch).expect("The current time is before the Unix Epoch!");
```

Abschließend können wir die `Duration` in das gewünschte `DateTime`-Format konvertieren und es ausgeben.

```Rust
use std::time::SystemTime;
use chrono::prelude::*;
let now = SystemTime::now();
let unix_epoch = UNIX_EPOCH;
let duration = now.duration_since(unix_epoch).expect("The current time is before the Unix Epoch!");
let current_datetime = NaiveDateTime::from_timestamp(duration.as_secs() as i64, 0);
println!("{}", current_datetime); // Beispielausgabe: 2021-01-13 11:30:45
```

## Deep Dive

Das `std::time`-Modul bietet auch andere nützliche Funktionen für die Arbeit mit Zeiten und Datum, wie zum Beispiel `Local` oder `Instant`. Außerdem gibt es das externe Paket `chrono`, das eine Vielzahl an Datentypen und Methoden speziell für das Arbeiten mit Datum und Zeit in Rust zur Verfügung stellt.

## Siehe auch

- [Rust Standardbibliothek](https://doc.rust-lang.org/std/)
- [Chrono-Dokumentation](https://docs.rs/chrono/)