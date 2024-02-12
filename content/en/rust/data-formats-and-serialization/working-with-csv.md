---
title:                "Working with CSV"
aliases:
- /en/rust/working-with-csv/
date:                  2024-02-03T19:03:28.265808-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Working with CSV (Comma-Separated Values) files is about reading from and writing to plain text files that store tabular data. Programmers do this to enable data sharing between different programs, systems, or for processing large data sets in an efficient, human-readable format.

## How to:
Rust, with its focus on safety and performance, offers excellent crates (libraries) for dealing with CSV files, with `csv` being the most popular. You'll also need `serde` for serializing and deserializing data.

First, add the dependencies to your `Cargo.toml`:

```toml
[dependencies]
csv = "1.1"
serde = { version = "1.0", features = ["derive"] }
```

### Reading CSV

To read a CSV file, define a struct that represents your data and derive `Deserialize` from `serde`:

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

Sample output for a CSV with city information might look like:
```plaintext
Record { city: "Seattle", state: "WA", population: 744955 }
Record { city: "New York", state: "NY", population: 8336817 }
```

### Writing to CSV

To write to a CSV file, define a struct and derive `Serialize`:

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

This will create `output.csv` with the data:

```csv
city,state,population
Los Angeles,CA,3979563
Chicago,IL,2695598
```

By leveraging Rust’s powerful type system and the ecosystem's robust crates, working with CSV data becomes both efficient and straightforward, ensuring safety and performance in your data processing tasks.
