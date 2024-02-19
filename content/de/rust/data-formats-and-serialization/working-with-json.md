---
aliases:
- /de/rust/working-with-json/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:59.508376-07:00
description: "Die Arbeit mit JSON (JavaScript Object Notation) in Rust dreht sich\
  \ um das Parsen von JSON-Daten in Rust-Datenstrukturen und das Serialisieren von\
  \ Rust-\u2026"
lastmod: 2024-02-18 23:09:04.664567
model: gpt-4-0125-preview
summary: "Die Arbeit mit JSON (JavaScript Object Notation) in Rust dreht sich um das\
  \ Parsen von JSON-Daten in Rust-Datenstrukturen und das Serialisieren von Rust-\u2026"
title: Arbeiten mit JSON
---

{{< edit_this_page >}}

## Was & Warum?

Die Arbeit mit JSON (JavaScript Object Notation) in Rust dreht sich um das Parsen von JSON-Daten in Rust-Datenstrukturen und das Serialisieren von Rust-Datenstrukturen zurück in JSON. Programmierer tun dies, um mit Web-APIs, Konfigurationsdateien oder jedem Datenaustauschformat zu interagieren, bei dem JSON aufgrund seines leichten und menschenlesbaren Formats verwendet wird.

## Wie:

Um mit JSON in Rust zu arbeiten, wird die `serde`-Crate zusammen mit `serde_json` für die Serialisierung und Deserialisierung umfassend genutzt. Zuerst stellen Sie sicher, dass diese in Ihrer `Cargo.toml` enthalten sind:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

### Beispiel 1: Deserialisieren von JSON zu einer Rust-Struktur

Definieren Sie eine Rust-Struktur und verwenden Sie Derive-Makros für `Deserialize` und `Serialize`:

```rust
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct User {
    id: u32,
    name: String,
    email: String,
}

fn main() {
    let json_data = r#"
        {
            "id": 1,
            "name": "Jane Doe",
            "email": "jane.doe@example.com"
        }
    "#;

    let user: User = serde_json::from_str(json_data).unwrap();

    println!("Benutzer-ID: {}", user.id);
    println!("Benutzername: {}", user.name);
    println!("Benutzer-E-Mail: {}", user.email);
}
```

**Ausgabe:**

```
Benutzer-ID: 1
Benutzername: Jane Doe
Benutzer-E-Mail: jane.doe@example.com
```

### Beispiel 2: Serialisieren einer Rust-Struktur zu JSON

Unter Verwendung derselben `User`-Struktur:

```rust
let user = User {
    id: 1,
    name: "Jane Doe".to_string(),
    email: "jane.doe@example.com".to_string(),
};

let json_data = serde_json::to_string(&user).unwrap();

println!("{}", json_data);
```

**Ausgabe:**

```json
{"id":1,"name":"Jane Doe","email":"jane.doe@example.com"}
```

Diese Beispiele demonstrieren den grundlegenden Ablauf des Deserialisierens von JSON in Rust-Strukturen und des Serialisierens von Rust-Strukturen zurück in JSON-Strings. Serde bietet einen umfangreichen Satz an Werkzeugen für die Arbeit mit JSON, einschließlich des Umgangs mit optionalen Feldern, komplexer Verschachtelung und Typen, die nicht direkt von JSON unterstützt werden.
