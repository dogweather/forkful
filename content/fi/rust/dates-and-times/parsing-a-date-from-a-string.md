---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:45.256711-07:00
description: "Miten: Rustin standardikirjasto ei sis\xE4ll\xE4 suoraan p\xE4iv\xE4\
  m\xE4\xE4r\xE4n j\xE4sent\xE4mist\xE4, mutta laajasti k\xE4ytetty `chrono`-paketti\
  \ on vankka ratkaisu p\xE4iv\xE4m\xE4\xE4rien ja\u2026"
lastmod: '2024-03-13T22:44:56.366197-06:00'
model: gpt-4-0125-preview
summary: "Rustin standardikirjasto ei sis\xE4ll\xE4 suoraan p\xE4iv\xE4m\xE4\xE4r\xE4\
  n j\xE4sent\xE4mist\xE4, mutta laajasti k\xE4ytetty `chrono`-paketti on vankka ratkaisu\
  \ p\xE4iv\xE4m\xE4\xE4rien ja ajan k\xE4sittelyyn."
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sennys merkkijonosta"
weight: 30
---

## Miten:


### Käyttäen Rustin Standardikirjastoa (`chrono`-paketti)
Rustin standardikirjasto ei sisällä suoraan päivämäärän jäsentämistä, mutta laajasti käytetty `chrono`-paketti on vankka ratkaisu päivämäärien ja ajan käsittelyyn. Lisää ensin `chrono` `Cargo.toml`-tiedostoosi:

```toml
[dependencies]
chrono = "0.4"
```

Käytä sitten `chrono`-pakettia jäsentämään päivämäärämerkkijono `NaiveDate`-olioksi:

```rust
extern crate chrono;
use chrono::NaiveDate;

fn main() {
    let date_str = "2023-04-01";
    let date = NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
        .expect("Päivämäärän jäsentäminen epäonnistui");

    println!("Jäsennetty päivämäärä: {}", date);
}

// Esimerkkituloste:
// Jäsennetty päivämäärä: 2023-04-01
```

### Käyttäen Rustin Edistynyttä Päivämäärä-Ajan Käsittelyä (`time`-paketti)
Edistyneemmän päivämäärä-ajan käsittelyn osalta, mukaan lukien ergonomisempi jäsentäminen, harkitse `time`-paketin käyttöä. Lisää se ensin `Cargo.toml`-tiedostoosi:

```toml
[dependencies]
time = "0.3"
```

Jäsennä sitten päivämäärämerkkijono käyttäen `Date`-tyyppiä ja `PrimitiveDateTime`-oliota:

```rust
use time::{Date, PrimitiveDateTime, macros::datetime};

fn main() {
    let date_str = "2023-04-01 12:34:56";
    let parsed_date = PrimitiveDateTime::parse(
        date_str, 
        &datetime!("%Y-%m-%d %H:%M:%S")
    ).expect("Päivämäärän ja ajan jäsentäminen epäonnistui");

    println!("Jäsennetty päivämääräaika: {}", parsed_date);
}

// Esimerkkituloste:
// Jäsennetty päivämääräaika: 2023-04-01 12:34:56
```

Molemmat esimerkit havainnollistavat, miten Rust kolmannen osapuolen pakettien avulla helpottaa päivämäärämerkkijonojen jäsentämistä käsiteltäviksi päivämääräolioiksi, tehden siitä tehokkaan työkalun ohjelmistokehitykseen, joka sisältää ajallista dataa.
