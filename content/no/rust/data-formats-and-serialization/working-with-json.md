---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:09.788065-07:00
description: "Hvordan: For \xE5 arbeide med JSON i Rust, er `serde`-craten sammen\
  \ med `serde_json` for serialisering og deserialisering omfattende brukt. F\xF8\
  rst, s\xF8rg for \xE5\u2026"
lastmod: '2024-03-13T22:44:40.595381-06:00'
model: gpt-4-0125-preview
summary: "For \xE5 arbeide med JSON i Rust, er `serde`-craten sammen med `serde_json`\
  \ for serialisering og deserialisering omfattende brukt."
title: Arbeider med JSON
weight: 38
---

## Hvordan:
For å arbeide med JSON i Rust, er `serde`-craten sammen med `serde_json` for serialisering og deserialisering omfattende brukt. Først, sørg for å inkludere disse i din `Cargo.toml`:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

### Eksempel 1: Deserialisere JSON til en Rust Struct
Definer en Rust struct og bruk derive-makroer for `Deserialize` og `Serialize`:

```rust
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct Bruker {
    id: u32,
    navn: String,
    epost: String,
}

fn main() {
    let json_data = r#"
        {
            "id": 1,
            "navn": "Jane Doe",
            "epost": "jane.doe@example.com"
        }
    "#;

    let bruker: Bruker = serde_json::from_str(json_data).unwrap();

    println!("Bruker ID: {}", bruker.id);
    println!("Bruker Navn: {}", bruker.navn);
    println!("Bruker Epost: {}", bruker.epost);
}
```

**Output:**

```
Bruker ID: 1
Bruker Navn: Jane Doe
Bruker Epost: jane.doe@example.com
```

### Eksempel 2: Serialisere en Rust Struct til JSON
Ved å bruke den samme `Bruker`-structen:

```rust
let bruker = Bruker {
    id: 1,
    navn: "Jane Doe".to_string(),
    epost: "jane.doe@example.com".to_string(),
};

let json_data = serde_json::to_string(&bruker).unwrap();

println!("{}", json_data);
```

**Output:**

```json
{"id":1,"navn":"Jane Doe","epost":"jane.doe@example.com"}
```

Disse eksemplene demonstrerer den grunnleggende flyten av deserialisering av JSON til Rusts strukturer og serialisering av Rusts strukturer tilbake til JSON-strenger. Serde tilbyr et rikt sett med verktøy for å arbeide med JSON, inkludert å håndtere valgfrie felt, kompleks nesting og typer som ikke er direkte støttet av JSON.
