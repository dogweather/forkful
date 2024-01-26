---
title:                "Arbeid med CSV"
html_title:           "Bash: Arbeid med CSV"
simple_title:         "Arbeid med CSV"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Arbeid med CSV (Comma-Separated Values) håndterer data i en tabellformet format som er enkel å lese og skrive for både mennesker og maskiner. Programmerere bruker CSV fordi det er effektivt for lagring og utveksling av store datamengder mellom forskjellige programmer.

## Hvordan gjøre det:

Rust gir deg kraftfulle verktøy for å jobbe med CSV-filer. Her er et eksempel som bruker `csv`-biblioteket:

```rust
use csv::{ReaderBuilder, WriterBuilder};
use std::error::Error;
use std::io;

fn lese_og_skrive_csv() -> Result<(), Box<dyn Error>> {
    let data = "
land,hovedstad
Norge,Oslo
Sverige,Stockholm
";
    
    let mut reader = ReaderBuilder::new().from_reader(data.as_bytes());
    let mut writer = WriterBuilder::new().from_writer(io::stdout());
    
    for record in reader.records() {
        let record = record?;
        writer.write_record(&record)?;
    }
    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    lese_og_skrive_csv()
}
```

Etter kjøring vil utskriften være:

```
land,hovedstad
Norge,Oslo
Sverige,Stockholm
```

## Dypdykk

CSV-formatet ble populært på 1970-tallet og er enkelt fordi det kun bruker komma for å skille verdier og ny linje for nye rader. Alternativer til CSV inkluderer JSON, XML og databaser som SQLite, men CSV forblir populært på grunn av sin enkelhet og bred støtte. Ved implementering i Rust, håndterer `csv`-biblioteket parsing og skriving, og tar seg av feil som manglende felt eller feil datatyper.

## Se også

1. [Rust `csv` crate dokumentasjon](https://docs.rs/csv/latest/csv/)
2. [Rust Programming Language offisielle nettsted](https://www.rust-lang.org/)
3. [RFC 4180, som definerer CSV standardformatet](https://tools.ietf.org/html/rfc4180)
4. [Serde-biblioteket for data serialisering/deserialisering](https://serde.rs/)
