---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:23.488824-07:00
description: "Wie man das macht: Rust, mit seinem Fokus auf Sicherheit und Leistung,\
  \ bietet ausgezeichnete Crates (Bibliotheken) f\xFCr die Arbeit mit CSV-Dateien,\
  \ wobei\u2026"
lastmod: '2024-03-13T22:44:53.695407-06:00'
model: gpt-4-0125-preview
summary: "Rust, mit seinem Fokus auf Sicherheit und Leistung, bietet ausgezeichnete\
  \ Crates (Bibliotheken) f\xFCr die Arbeit mit CSV-Dateien, wobei `csv` das beliebteste\
  \ ist."
title: Arbeiten mit CSV
weight: 37
---

## Wie man das macht:
Rust, mit seinem Fokus auf Sicherheit und Leistung, bietet ausgezeichnete Crates (Bibliotheken) für die Arbeit mit CSV-Dateien, wobei `csv` das beliebteste ist. Du benötigst auch `serde` zum Serialisieren und Deserialisieren von Daten.

Zuerst füge die Abhängigkeiten zu deiner `Cargo.toml` hinzu:

```toml
[dependencies]
csv = "1.1"
serde = { version = "1.0", features = ["derive"] }
```

### CSV lesen
Um eine CSV-Datei zu lesen, definiere eine Struktur, die deine Daten repräsentiert, und leite `Deserialize` von `serde` ab:

```rust
use serde::Deserialize;
use std::error::Error;
use std::fs::File;
use std::io;
use std::process;

#[derive(Debug, Deserialize)]
struct Record {
    city: String,
    state: String,
    population: u64,
}

fn read_from_csv(file_path: &str) -> Result<(), Box<dyn Error>> {
    let file = File::open(file_path)?;
    let mut rdr = csv::Reader::from_reader(file);

    for result in rdr.deserialize() {
        let record: Record = result?;
        println!("{:?}", record);
    }
    Ok(())
}

fn main() {
    if let Err(err) = read_from_csv("cities.csv") {
        println!("Fehler beim Ausführen des Beispiels: {}", err);
        process::exit(1);
    }
}
```

Eine beispielhafte Ausgabe für eine CSV mit Stadtinformationen könnte wie folgt aussehen:
```plaintext
Record { city: "Seattle", state: "WA", population: 744955 }
Record { city: "New York", state: "NY", population: 8336817 }
```

### In CSV schreiben
Um in eine CSV-Datei zu schreiben, definiere eine Struktur und leite `Serialize` ab:

```rust
use serde::Serialize;
use std::error::Error;
use std::fs::File;

#[derive(Serialize)]
struct Record {
    city: String,
    state: String,
    population: u64,
}

fn write_to_csv(file_path: &str, records: Vec<Record>) -> Result<(), Box<dyn Error>> {
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
            city: "Los Angeles".into(),
            state: "CA".into(),
            population: 3979563,
        },
        Record {
            city: "Chicago".into(),
            state: "IL".into(),
            population: 2695598,
        },
    ];

    write_to_csv("output.csv", records)?;

    Ok(())
}
```

Dies erstellt `output.csv` mit den Daten:

```csv
city,state,population
Los Angeles,CA,3979563
Chicago,IL,2695598
```

Durch die Nutzung des leistungsfähigen Typensystems von Rust und der robusten Crates des Ökosystems wird die Arbeit mit CSV-Daten sowohl effizient als auch unkompliziert, und gewährleistet Sicherheit und Leistung bei deinen Datenverarbeitungsaufgaben.
