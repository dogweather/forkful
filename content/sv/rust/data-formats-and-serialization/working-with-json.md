---
title:                "Arbeta med JSON"
aliases:
- sv/rust/working-with-json.md
date:                  2024-02-03T19:24:05.488051-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeta med JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att arbeta med JSON (JavaScript Object Notation) i Rust handlar om att tolka JSON-data till Rusts datastrukturer och serialisera Rusts datastrukturer tillbaka till JSON. Programmerare gör detta för att interagera med webb-APIer, konfigurationsfiler eller något datautbytesformat där JSON används på grund av dess lätta och lättlästa format.

## Hur man gör:

För att arbeta med JSON i Rust används `serde`-craten tillsammans med `serde_json` för serialisering och deserialisering flitigt. Börja med att inkludera dessa i din `Cargo.toml`:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

### Exempel 1: Deserialisera JSON till en Rust-struktur

Definiera en Rust-struktur och använd makron för `Deserialize` och `Serialize`:

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

    println!("Användar-ID: {}", user.id);
    println!("Användarnamn: {}", user.name);
    println!("Användaremail: {}", user.email);
}
```

**Utskrift:**

```
Användar-ID: 1
Användarnamn: Jane Doe
Användaremail: jane.doe@example.com
```

### Exempel 2: Serialisera en Rust-struktur till JSON

Med samma `User`-struktur:

```rust
let user = User {
    id: 1,
    name: "Jane Doe".to_string(),
    email: "jane.doe@example.com".to_string(),
};

let json_data = serde_json::to_string(&user).unwrap();

println!("{}", json_data);
```

**Utskrift:**

```json
{"id":1,"name":"Jane Doe","email":"jane.doe@example.com"}
```

Dessa exempel demonstrerar det grundläggande flödet av deserialisering av JSON till Rust-strukturer och serialisering av Rust-strukturer tillbaka till JSON-strängar. Serde erbjuder en rik uppsättning verktyg för att arbeta med JSON, inklusive hantering av valfria fält, komplex nästling och typer som inte direkt stöds av JSON.
