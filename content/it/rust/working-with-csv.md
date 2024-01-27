---
title:                "Lavorare con i file CSV"
date:                  2024-01-19
html_title:           "Bash: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Lavorare con CSV significa manipolare dati in formato "Comma-Separated Values", comune in applicazioni di data-exchange. I programmatori lo usano per la sua semplicità e interoperabilità fra sistemi diversi.

## How to:
Installare la crate `csv` e `serde` nel tuo `Cargo.toml`:

```toml
[dependencies]
csv = "1.1"
serde = { version = "1.0", features = ["derive"] }
```

Leggi un CSV da un file e stampa ogni record:

```rust
use csv;
use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct Record {
    nome: String,
    città: String,
    eta: u8,
}

fn main() -> Result<(), csv::Error> {
    let mut rdr = csv::Reader::from_path("dati.csv")?;
    
    for result in rdr.deserialize() {
        let record: Record = result?;
        println!("{:?}", record);
    }
    
    Ok(())
}
```

Output d'esempio:

```plaintext
Record { nome: "Mario", città: "Roma", eta: 35 }
Record { nome: "Luca", città: "Milano", eta: 22 }
```

## Deep Dive
CSV è nato negli anni '70, conosciuto per la sua compatibilità con fogli elettronici e database. Alternative includono JSON e XML, che offrono strutture dati più complesse, ma con maggiore overhead. In Rust, lavorare con CSV è efficiente grazie alla tipizzazione statica e al sistema di serializzazione/deserializzazione offerto da `serde`.

## See Also
- Documentazione ufficiale di `csv`: https://docs.rs/csv/latest/csv/
- Serde, un framework per serializzare dati in Rust: https://serde.rs/
- Perché usare CSV?: https://www.why-csv.club/
