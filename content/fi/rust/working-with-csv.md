---
title:                "CSV-tiedostojen käsittely"
html_title:           "Bash: CSV-tiedostojen käsittely"
simple_title:         "CSV-tiedostojen käsittely"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV (Comma-Separated Values) on datan tallennusmuoto, jossa data on eritelty pilkuilla erotettuina arvoina. Ohjelmoijat käyttävät CSV:tä, koska se on yksinkertainen ja yleisesti yhteensopiva eri ohjelmien ja kielten kanssa.

## How to:
Rustilla voi lukea ja kirjoittaa CSV-tiedostoja `csv`-kirjaston avulla. Asenna ensin kirjasto lisäämällä `Cargo.toml`-tiedostoosi:
```toml
[dependencies]
csv = "1.1"
```

Lue CSV:
```rust
use csv::Reader;
use std::error::Error;

fn lue_csv() -> Result<(), Box<dyn Error>> {
    let mut rdr = Reader::from_path("data.csv")?;
    for tulos in rdr.records() {
        let record = tulos?;
        println!("{:?}", record);
    }
    Ok(())
}

fn main() {
    if let Err(err) = lue_csv() {
        println!("Virhe: {}", err);
    }
}
```

Kirjoita CSV:
```rust
use csv::Writer;
use std::error::Error;

fn kirjoita_csv() -> Result<(), Box<dyn Error>> {
    let mut wtr = Writer::from_path("tulos.csv")?;
    wtr.write_record(&["sarake1", "sarake2"])?;
    wtr.write_record(&["arvo1", "arvo2"])?;
    wtr.flush()?;
    Ok(())
}

fn main() {
    if let Err(err) = kirjoita_csv() {
        println!("Virhe: {}", err);
    }
}
```
## Deep Dive
CSV-formaatti syntyi varhain tietokoneiden historiassa helpottamaan taulukkomuotoisen datan siirtämistä ohjelmien välillä. Se ei ole yhtä joustava kuin JSON tai XML, mutta sen yksinkertaisuus tekee siitä sopivan raa'an datan käsittelyyn ja siirtoon. Rustissa `csv`-kirjasto tarjoaa monipuoliset työkalut CSV-tiedostojen hallintaan, kuten iteratorit ja ser/deserialisaatio mukautettujen tietueiden kanssa.

## See Also
- Rust `csv`-kirjasto: https://docs.rs/csv/
- CSV-formaatin RFC 4180: https://tools.ietf.org/html/rfc4180
- Rust-ohjelmointikielen virallinen sivusto: https://www.rust-lang.org/