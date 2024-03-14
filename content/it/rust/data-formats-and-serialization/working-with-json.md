---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:07.227746-07:00
description: "Lavorare con JSON (JavaScript Object Notation) in Rust consiste nell'analizzare\
  \ i dati JSON per trasformarli in strutture dati Rust e serializzare le\u2026"
lastmod: '2024-03-13T22:44:43.239954-06:00'
model: gpt-4-0125-preview
summary: "Lavorare con JSON (JavaScript Object Notation) in Rust consiste nell'analizzare\
  \ i dati JSON per trasformarli in strutture dati Rust e serializzare le\u2026"
title: Lavorare con JSON
---

{{< edit_this_page >}}

## Cosa & Perché?

Lavorare con JSON (JavaScript Object Notation) in Rust consiste nell'analizzare i dati JSON per trasformarli in strutture dati Rust e serializzare le strutture dati Rust di nuovo in JSON. I programmatori lo fanno per interagire con API web, file di configurazione o qualsiasi formato di scambio dati dove viene utilizzato JSON grazie al suo formato leggero e facilmente leggibile.

## Come fare:

Per lavorare con JSON in Rust, si utilizza estensivamente la crate `serde` insieme a `serde_json` per la serializzazione e deserializzazione. Prima di tutto, assicurati di includerle nel tuo `Cargo.toml`:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

### Esempio 1: Deserializzare JSON in una Struct Rust

Definisci una struct Rust e usa le macro derive per `Deserialize` e `Serialize`:

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

    println!("ID Utente: {}", user.id);
    println!("Nome Utente: {}", user.name);
    println!("Email Utente: {}", user.email);
}
```

**Output:**

```
ID Utente: 1
Nome Utente: Jane Doe
Email Utente: jane.doe@example.com
```

### Esempio 2: Serializzare una Struct Rust in JSON

Utilizzando la stessa struct `User`:

```rust
let user = User {
    id: 1,
    name: "Jane Doe".to_string(),
    email: "jane.doe@example.com".to_string(),
};

let json_data = serde_json::to_string(&user).unwrap();

println!("{}", json_data);
```

**Output:**

```json
{"id":1,"name":"Jane Doe","email":"jane.doe@example.com"}
```

Questi esempi dimostrano il flusso di base per deserializzare JSON in strutture Rust e serializzare strutture Rust di nuovo in stringhe JSON. Serde offre un ricco insieme di strumenti per lavorare con JSON, incluso il trattamento di campi opzionali, nidificazioni complesse e tipi non direttamente supportati da JSON.
