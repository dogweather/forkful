---
date: 2024-01-20 17:37:17.325480-07:00
description: "Anleitung: Historisch wurde `strftime` in C genutzt, um Datumszeichenketten\
  \ zu formatieren. Rusts `chrono` Crate bietet \xE4hnliche Funktionalit\xE4t mit\u2026"
lastmod: '2024-04-05T21:53:55.563807-06:00'
model: gpt-4-1106-preview
summary: Historisch wurde `strftime` in C genutzt, um Datumszeichenketten zu formatieren.
title: Datum in einen String umwandeln
weight: 28
---

## Anleitung:
```Rust
use chrono::{DateTime, Utc, Local};

fn main() {
    // Aktuelles Datum und Uhrzeit in UTC
    let utc_date: DateTime<Utc> = Utc::now();
    println!("{}", utc_date.format("%d.%m.%Y %H:%M:%S").to_string()); // 24.12.2023 17:00:00

    // Aktuelles Datum und Uhrzeit lokal
    let local_date: DateTime<Local> = Local::now();
    println!("{}", local_date.format("%d.%m.%Y %H:%M:%S").to_string()); // 24.12.2023 19:00:00
}
```
Ausgabe:
```
24.12.2023 17:00:00
24.12.2023 19:00:00
```

## Tiefgang:
Historisch wurde `strftime` in C genutzt, um Datumszeichenketten zu formatieren. Rusts `chrono` Crate bietet ähnliche Funktionalität mit `format`. Alternativen in anderen Sprachen sind `DateTime.ToString()` in C# oder `date.strftime` in Python. Performance-wise, Rusts `chrono` Formatierung ist effizient, da sie zur Compile-Zeit geprüft wird und typsicher ist, was zur Laufzeitsicherheit beiträgt.

## Siehe Auch:
- Rust `chrono` crate: https://docs.rs/chrono/
- Rust Date & Time Formatierung: https://docs.rs/chrono/0.4.19/chrono/format/strftime/index.html
- strftime Formatierungs-Optionen: https://man7.org/linux/man-pages/man3/strftime.3.html
