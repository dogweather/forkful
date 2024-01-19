---
title:                "Konvertere en dato til en streng"
html_title:           "Arduino: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Konverter Datoverdier til Strenger i Rust

## Hva og Hvorfor?

Å konvertere en dato til en streng betyr å endre datatypen fra date til string. Dette hjelper programmerere med å manipulere datoer lettere, gjøre sammenligninger og presentere dataene på et mer lesbart format.

## Hvordan Gjør Det:

I Rust programmeringsspråk kan vi ta i bruk Chrono biblioteket. Installere det med følgende kode i din `Cargo.toml`:

```Rust
[dependencies]
chrono = "0.4"
```

Enkel konvertering vil se ut slik:

```Rust
extern crate chrono;
use chrono::{DateTime, Utc};

fn main() {
    let nå: DateTime<Utc> = Utc::now(); //Få nåværende tid
    println!("{}", nå.to_string()); //Print nåværende dato og tid
}
```

Kjør programmet og du får dette resultatet:

```Rust
2023-09-27 08:57:11.005998800 UTC
```

## Dypdykk

Historisk sett, er konvertering av datoverdier til strenger ikke unikt for Rust. Mesteparten av programmeringsspråkene har funksjoner for å håndtere dette, gitt viktigheten av å kunne manipulere og presentere dato og tid.

Som et alternativ, kan du bruke `.format()` funksjonen til å spesifisere formatet på dato-strengen:

```Rust
extern crate chrono;
use chrono::{DateTime, Utc};

fn main() {
    let nå: DateTime<Utc> = Utc::now(); //Få nåværende tid
    println!("{}", nå.format("%d-%m-%Y %H:%M:%S").to_string()); //Print dato og tid i spesifisert format
}
```
Denne vil returnere datoverdi som en streng formatert slik:

```Rust
27-09-2023 08:57:11
```

Til syvende og sist, vil teknikken du bruker avhenge av dine krav og hva du foretrekker.

## Se også:

1. [Chrono biblioteket på crates.io](https://crates.io/crates/chrono)
2. [Dokumentasjonen for `chrono::format::strftime` på Rust](https://docs.rs/chrono/0.4.19/chrono/format/strftime/index.html)