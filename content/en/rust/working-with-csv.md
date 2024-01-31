---
title:                "Working with CSV"
date:                  2024-01-19
simple_title:         "Working with CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

CSV, short for Comma-Separated Values, is a file format used to store tabular data. Programmers love CSV for its simplicity and widespread support across tools and programming languages for data manipulation, import, and export.

## How to:

First, include the necessary crate in `Cargo.toml`:

```toml
[dependencies]
csv = "1.1"
```

Then, handle reading a CSV:

```rust
use csv::Reader;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let mut rdr = Reader::from_path("data.csv")?;
    for result in rdr.records() {
        let record = result?;
        println!("{:?}", record);
    }
    Ok(())
}
```

Write to a CSV:

```rust
use csv::Writer;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let mut wtr = Writer::from_path("output.csv")?;
    wtr.write_record(&["name", "city", "age"])?;
    wtr.write_record(&["Jane", "New York", "30"])?;
    wtr.flush()?;
    Ok(())
}
```

Sample output for reading:

```
StringRecord(["Jane", "New York", "30"])
```

## Deep Dive

CSV has been around since the early days of personal computing, used for exchanging data between programs and systems. While JSON and XML provide more structure, CSV remains popular for its light weight and ease of use.

Alternatives to csv crate in Rust include `serde_csv`, offering convenient serialization and deserialization, and `papercut`, focusing on safe and ergonomic CSV parsing.

CSV parsing in Rust is I/O bound. Efficient handling involves using iterators and Rust's robust error handling to manage malformed data.

## See Also

- Rust CSV crate documentation: https://docs.rs/csv/
- The Rust Programming Language book: https://doc.rust-lang.org/book/
- Serde: https://serde.rs/ - a framework for serializing and deserializing Rust data structures.
- Rust by Example CSV: https://rustbyexample.com/std_misc/file/csv.html
