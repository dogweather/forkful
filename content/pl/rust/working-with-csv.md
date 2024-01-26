---
title:                "Praca z plikami CSV"
html_title:           "Bash: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
W pracy z danymi często spotkasz CSV – łatwy do odczytu i prosty format przechowywania tabelarycznych danych. Programiści korzystają z niego, gdy chcą szybko wymieniać i manipulować danymi między różnymi aplikacjami.

## How to:
Do obsługi CSV w Rust użyjemy crate'a `csv`. Najpierw dodaj zależność do `Cargo.toml`:

```toml
[dependencies]
csv = "1.1.6"
```

Teraz możesz czytać i pisać CSV:

```rust
use csv;
use std::error::Error;
use std::io;
use std::process;

fn main() {
    if let Err(err) = read_csv() {
        println!("Błąd przy czytaniu CSV: {}", err);
        process::exit(1);
    }
}

fn read_csv() -> Result<(), Box<dyn Error>> {
    let data = "imię,nazwisko,miasto
                Jan,Kowalski,Warszawa
                Maria,Nowak,Kraków";

    let mut rdr = csv::Reader::from_reader(data.as_bytes());

    for result in rdr.records() {
        let record = result?;
        println!("{:?}", record);
    }

    Ok(())
}
```

Wynik:
```
StringRecord(["imię", "nazwisko", "miasto"])
StringRecord(["Jan", "Kowalski", "Warszawa"])
StringRecord(["Maria", "Nowak", "Kraków"])
```

## Deep Dive:
CSV (Comma-Separated Values) istnieje od lat 70-tych. Alternatywami mogą być JSON lub XML, ale CSV jest prostsze. Przy obsłudze CSV w Rust należy pamiętać o handlerowaniu błędów, kwotowaniu tekstu i obsłudze różnych kodowań znaków.

## See Also:
- Oficjalna dokumentacja crate'a CSV: https://docs.rs/csv
- Rust by Example - obsługa plików: https://doc.rust-lang.org/rust-by-example/std_misc/file.html 
- Projektowanie API w Rust dla bardziej skomplikowanych użyć CSV: https://aturon.github.io/features/design/CSV-analysis/
