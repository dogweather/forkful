---
title:                "Working with csv"
html_title:           "Rust recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV stands for Comma Separated Values, which is a simple text format used to store tabular data. It consists of rows and columns, with each row representing a record and each column representing a field. Programmers work with CSV files to easily manipulate and analyze large data sets, as it allows for easy import and export of data between various applications and programming languages.

## How to:
To work with CSV files in Rust, you can use the rust-csv crate. Here's a simple example of how to read and print the data from a CSV file:

```rust
extern crate csv;

use std::error::Error;
use std::path::Path;
use csv::{ReaderBuilder, ErrorKind};

fn main() {
    let path = Path::new("data.csv");
    let file = match File::open(path) {
        Ok(file) => file,
        Err(e) => panic!("Error opening file: {}", e.description()),
    };

    let mut reader = ReaderBuilder::new()
        .has_records_impl(false)
        .from_reader(file);

    for result in reader.records() {
        match result {
            Err(err) => { println!("Error: {:?}", err.kind()); },
            Ok(record) => { println!("Record: {:?}", record); },
        }
    }
}
```

Running this code will print each record in the CSV file.

## Deep Dive:
CSV is a popular format for storing and exchanging data due to its simplicity and compatibility across different platforms and systems. It was first introduced in the 1970s and has since become a standard for data exchange in various industries such as finance, healthcare, and research.

There are also alternative formats for storing tabular data, such as JSON and XML. However, CSV remains a preferred choice for many programmers due to its ease of use and efficiency.

The rust-csv crate uses rust's built-in error handling library, which allows for easy and efficient handling of errors. It also supports various custom options for reading and writing CSV data, giving programmers more control over their data manipulation.

## See Also:
- Official rust-csv crate documentation: https://docs.rs/csv/
- Tutorial on working with CSV files in Rust: https://docs.rs/csv/1.0.0-beta.2/csv/tutorial/index.html
- Other popular CSV parsers in Rust: https://lib.rs/crates/csv