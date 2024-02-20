---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:27.176692-07:00
description: "CSV-tiedostojen (pilkuin erotetut arvot) k\xE4sittely tarkoittaa tabulaarisen\
  \ datan tallentavien yksinkertaistettujen tekstitiedostojen lukemista ja\u2026"
lastmod: 2024-02-19 22:05:15.274723
model: gpt-4-0125-preview
summary: "CSV-tiedostojen (pilkuin erotetut arvot) k\xE4sittely tarkoittaa tabulaarisen\
  \ datan tallentavien yksinkertaistettujen tekstitiedostojen lukemista ja\u2026"
title: "Ty\xF6skentely CSV:n kanssa"
---

{{< edit_this_page >}}

## Mitä & Miksi?
CSV-tiedostojen (pilkuin erotetut arvot) käsittely tarkoittaa tabulaarisen datan tallentavien yksinkertaistettujen tekstitiedostojen lukemista ja kirjoittamista. Ohjelmoijat tekevät tämän mahdollistaakseen datan jakamisen eri ohjelmien, järjestelmien välillä tai käsitelläkseen suuria datajoukkoja tehokkaassa, ihmisen luettavassa muodossa.

## Kuinka tehdä:
Rust, keskittyen turvallisuuteen ja suorituskykyyn, tarjoaa erinomaisia kirjastoja (crates) CSV-tiedostojen käsittelyyn, joista `csv` on suosituin. Tarvitset myös `serde`-kirjaston datan serialisointiin ja deserialisointiin.

Lisää ensin riippuvuudet `Cargo.toml`-tiedostoosi:

```toml
[dependencies]
csv = "1.1"
serde = { version = "1.0", features = ["derive"] }
```

### CSV:n lukeminen

CSV-tiedoston lukemiseksi määrittele rakenne, joka edustaa dataasi ja johda `Deserialize` `serde`-kirjastosta:

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
        println!("virhe esimerkin suorituksessa: {}", err);
        process::exit(1);
    }
}
```

Esimerkkitulo CSV-tiedostosta kaupunkitiedoilla voi näyttää tältä:
```plaintext
Record { city: "Seattle", state: "WA", population: 744955 }
Record { city: "New York", state: "NY", population: 8336817 }
```

### Kirjoittaminen CSV:ään

CSV-tiedostoon kirjoittamiseksi määrittele rakenne ja johda `Serialize`:

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

Tämä luo `output.csv` tiedoston seuraavalla datalla:

```csv
city,state,population
Los Angeles,CA,3979563
Chicago,IL,2695598
```

Hyödyntämällä Rustin tehokasta tyypitysjärjestelmää ja ekosysteemin vahvoja kirjastoja, CSV-datan käsittelystä tulee sekä tehokasta että suoraviivaista, taaten turvallisuuden ja suorituskyvyn datan käsittelytehtävissäsi.
