---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:04.552937-07:00
description: "Come fare: Rust, con il suo focus sulla sicurezza e le prestazioni,\
  \ offre eccellenti crate (librerie) per gestire i file CSV, essendo `csv` il pi\xF9\
  \u2026"
lastmod: '2024-03-13T22:44:43.240950-06:00'
model: gpt-4-0125-preview
summary: "Rust, con il suo focus sulla sicurezza e le prestazioni, offre eccellenti\
  \ crate (librerie) per gestire i file CSV, essendo `csv` il pi\xF9 popolare."
title: Lavorare con i CSV
weight: 37
---

## Come fare:
Rust, con il suo focus sulla sicurezza e le prestazioni, offre eccellenti crate (librerie) per gestire i file CSV, essendo `csv` il più popolare. Avrai bisogno anche di `serde` per la serializzazione e deserializzazione dei dati.

Prima, aggiungi le dipendenze al tuo `Cargo.toml`:

```toml
[dependencies]
csv = "1.1"
serde = { version = "1.0", features = ["derive"] }
```

### Leggere CSV
Per leggere un file CSV, definisci una struct che rappresenti i tuoi dati e deriva `Deserialize` da `serde`:

```rust
use serde::Deserialize;
use std::error::Error;
use std::fs::File;
use std::io;
use std::process;

#[derive(Debug, Deserialize)]
struct Record {
    città: String,
    stato: String,
    popolazione: u64,
}

fn leggi_da_csv(file_path: &str) -> Result<(), Box<dyn Error>> {
    let file = File::open(file_path)?;
    let mut rdr = csv::Reader::from_reader(file);

    for result in rdr.deserialize() {
        let record: Record = result?;
        println!("{:?}", record);
    }
    Ok(())
}

fn main() {
    if let Err(err) = leggi_da_csv("città.csv") {
        println!("errore nell'esecuzione dell'esempio: {}", err);
        process::exit(1);
    }
}
```

Un esempio di output per un CSV con informazioni sulle città potrebbe essere:
```plaintext
Record { città: "Seattle", stato: "WA", popolazione: 744955 }
Record { città: "New York", stato: "NY", popolazione: 8336817 }
```

### Scrivere su CSV
Per scrivere su un file CSV, definisci una struct e deriva `Serialize`:

```rust
use serde::Serialize;
use std::error::Error;
use std::fs::File;

#[derive(Serialize)]
struct Record {
    città: String,
    stato: String,
    popolazione: u64,
}

fn scrivi_su_csv(file_path: &str, records: Vec<Record>) -> Result<(), Box<dyn Error>> {
    let file = File::create(file_path)?;
    let mut wtr = csv::Writer::from_writer(file);

    for record in records {
        wtr.serialize(&record)?;
    }
    wtr.flush()?;
    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let records = vec![
        Record {
            città: "Los Angeles".into(),
            stato: "CA".into(),
            popolazione: 3979563,
        },
        Record {
            città: "Chicago".into(),
            stato: "IL".into(),
            popolazione: 2695598,
        },
    ];

    scrivi_su_csv("output.csv", records)?;

    Ok(())
}
```

Ciò creerà `output.csv` con i dati:

```csv
città,stato,popolazione
Los Angeles,CA,3979563
Chicago,IL,2695598
```

Sfruttando il potente sistema di tipi di Rust e le robuste crate dell'ecosistema, lavorare con dati CSV diventa sia efficiente che semplice, garantendo sicurezza e prestazioni nelle tue attività di elaborazione dei dati.
