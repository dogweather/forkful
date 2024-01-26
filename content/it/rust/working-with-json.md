---
title:                "Lavorare con JSON"
html_title:           "Arduino: Lavorare con JSON"
simple_title:         "Lavorare con JSON"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Lavorare con JSON significa manipolare dati in un formato leggero di scambio dati. I programmatori lo fanno per l'interoperabilità tra sistemi e la facilità di uso con APIs e configurazioni.

## How to: (Come fare)
```Rust
use serde::{Deserialize, Serialize};
use serde_json::Result;

// Definisci una struttura che corrisponde ai tuoi dati JSON.
#[derive(Serialize, Deserialize)]
struct User {
    id: u64,
    name: String,
    email: String,
}

fn main() -> Result<()> {
    // Dati JSON da deserializzare.
    let data = r#"
        {
            "id": 1,
            "name": "Mario Rossi",
            "email": "mario.rossi@example.com"
        }"#;

    // Deserializza i dati JSON nella struttura User.
    let user: User = serde_json::from_str(data)?;

    // Stampa l'oggetto User.
    println!("ID: {}, Name: {}, Email: {}", user.id, user.name, user.email);

    // Serializza l'oggetto User in una stringa JSON.
    let j = serde_json::to_string(&user)?;

    // Stampa la stringa JSON.
    println!("{}", j);
    
    Ok(())
}

```

Output:
```
ID: 1, Name: Mario Rossi, Email: mario.rossi@example.com
{"id":1,"name":"Mario Rossi","email":"mario.rossi@example.com"}
```

## Deep Dive (Approfondimento)
JSON (JavaScript Object Notation) esiste dagli anni 2000, derivante da JavaScript ma ora indipendente. Alternatives includono XML e YAML, ma JSON è più leggero e veloce. In Rust, `serde_json` è il crate più utilizzato per la serializzazione e deserializzazione, fornendo vasta personalizzazione e performance ottimizzate.

## See Also (Vedi Anche)
- [Serde](https://serde.rs/): un framework per serializzare strutture dati in Rust.
- [Serde JSON documentation](https://docs.serde.rs/serde_json/): documentazione ufficiale del crate `serde_json`.
- [JSON Official Site](https://www.json.org/json-en.html): informazioni ufficiali e specifiche di JSON.
