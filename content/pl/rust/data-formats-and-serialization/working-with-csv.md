---
title:                "Praca z plikami CSV"
aliases:
- /pl/rust/working-with-csv.md
date:                  2024-02-03T19:21:36.392094-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z plikami CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Praca z plikami CSV (Comma-Separated Values - Wartości Rozdzielone Przecinkami) polega na odczytywaniu z i zapisywaniu do prostych plików tekstowych, które przechowują dane tabelaryczne. Programiści wykonują tę czynność w celu umożliwienia wymiany danych pomiędzy różnymi programami, systemami lub dla przetwarzania dużych zbiorów danych w efektywnym, czytelnym dla człowieka formacie.

## Jak to zrobić:
Rust, ze swoim naciskiem na bezpieczeństwo i wydajność, oferuje doskonałe crate'y (biblioteki) do pracy z plikami CSV, z `csv` jako najpopularniejszym. Będziesz także potrzebować `serde` do serializacji i deserializacji danych.

Na początek, dodaj zależności do swojego `Cargo.toml`:

```toml
[dependencies]
csv = "1.1"
serde = { version = "1.0", features = ["derive"] }
```

### Odczytywanie CSV

Aby odczytać plik CSV, zdefiniuj strukturę, która reprezentuje twoje dane i wywieź `Deserialize` z `serde`:

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
        println!("błąd podczas uruchamiania przykładu: {}", err);
        process::exit(1);
    }
}
```

Przykładowe wyjście dla CSV z informacjami o miastach może wyglądać tak:
```plaintext
Record { city: "Seattle", state: "WA", population: 744955 }
Record { city: "New York", state: "NY", population: 8336817 }
```

### Zapisywanie do CSV

Aby zapisać do pliku CSV, zdefiniuj strukturę i wywieź `Serialize`:

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

To stworzy `output.csv` z danymi:

```csv
city,state,population
Los Angeles,CA,3979563
Chicago,IL,2695598
```

Wykorzystując potężny system typów Rusta oraz solidne crate'y ekosystemu, praca z danymi CSV staje się zarówno efektywna, jak i prosta, zapewniając bezpieczeństwo i wydajność w zadaniach przetwarzania danych.
