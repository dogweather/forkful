---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:17.356845-07:00
description: "Hvordan: Rust, med sitt fokus p\xE5 sikkerhet og ytelse, tilbyr utmerkede\
  \ crates (biblioteker) for \xE5 h\xE5ndtere CSV-filer, hvor `csv` er det mest popul\xE6\
  re. Du\u2026"
lastmod: '2024-03-13T22:44:40.596424-06:00'
model: gpt-4-0125-preview
summary: "Rust, med sitt fokus p\xE5 sikkerhet og ytelse, tilbyr utmerkede crates\
  \ (biblioteker) for \xE5 h\xE5ndtere CSV-filer, hvor `csv` er det mest popul\xE6\
  re."
title: Arbeide med CSV
weight: 37
---

## Hvordan:
Rust, med sitt fokus på sikkerhet og ytelse, tilbyr utmerkede crates (biblioteker) for å håndtere CSV-filer, hvor `csv` er det mest populære. Du vil også trenge `serde` for serialisering og deserialisering av data.

Først, legg til avhengighetene i din `Cargo.toml`:

```toml
[dependencies]
csv = "1.1"
serde = { version = "1.0", features = ["derive"] }
```

### Å lese CSV
For å lese en CSV-fil, definer en struct som representerer dataene dine og utled `Deserialize` fra `serde`:

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
        println!("error running example: {}", err);
        process::exit(1);
    }
}
```

Eksempelutdata for en CSV med byinformasjon kan se slik ut:
```plaintext
Record { city: "Seattle", state: "WA", population: 744955 }
Record { city: "New York", state: "NY", population: 8336817 }
```

### Å skrive til CSV
For å skrive til en CSV-fil, definer en struct og utled `Serialize`:

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

Dette vil opprette `output.csv` med dataene:

```csv
city,state,population
Los Angeles,CA,3979563
Chicago,IL,2695598
```

Ved å utnytte Rusts kraftige typesystem og økosystemets robuste crates, blir arbeid med CSV-data både effektivt og greit, og sikrer både sikkerhet og ytelse i dine databehandlingsoppgaver.
