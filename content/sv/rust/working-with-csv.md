---
title:                "Arbeta med CSV"
aliases:
- sv/rust/working-with-csv.md
date:                  2024-02-03T19:21:33.326934-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeta med CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med CSV-filer (kommaseparerade värden) handlar om att läsa från och skriva till vanliga textfiler som lagrar tabelldata. Programmerare gör detta för att möjliggöra datadelning mellan olika program, system eller för att bearbeta stora datamängder på ett effektivt, läsbart format.

## Hur man gör:
Rust, med dess fokus på säkerhet och prestanda, erbjuder utmärkta crates (bibliotek) för att hantera CSV-filer, där `csv` är det mest populära. Du kommer också att behöva `serde` för serialisering och deserialisering av data.

Först, lägg till beroenden i din `Cargo.toml`:

```toml
[dependencies]
csv = "1.1"
serde = { version = "1.0", features = ["derive"] }
```

### Läsa CSV

För att läsa en CSV-fil, definiera en struktur som representerar dina data och härled `Deserialize` från `serde`:

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
    let mut rdr = csv::Reader::från_reader(file);

    for result in rdr.deserialize() {
        let record: Record = result?;
        println!("{:?}", record);
    }
    Ok(())
}

fn main() {
    if let Err(err) = read_from_csv("cities.csv") {
        println!("fel vid körning av exempel: {}", err);
        process::exit(1);
    }
}
```

Exempelutdata för en CSV med stadsinformation kan se ut så här:
```plaintext
Record { city: "Seattle", state: "WA", population: 744955 }
Record { city: "New York", state: "NY", population: 8336817 }
```

### Skriva till CSV

För att skriva till en CSV-fil, definiera en struktur och härled `Serialize`:

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
    let mut wtr = csv::Writer::från_writer(file);

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

Detta skapar `output.csv` med datan:

```csv
city,state,population
Los Angeles,CA,3979563
Chicago,IL,2695598
```

Genom att utnyttja Rusts kraftfulla typsystem och ekosystemets robusta crates, blir arbetet med CSV-data både effektivt och enkelt, vilket säkerställer säkerhet och prestanda i dina databearbetningsuppgifter.
