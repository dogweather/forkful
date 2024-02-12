---
title:                "Päivämäärän jäsennys merkkijonosta"
aliases:
- /fi/rust/parsing-a-date-from-a-string/
date:                  2024-02-03T19:15:45.256711-07:00
model:                 gpt-4-0125-preview
simple_title:         "Päivämäärän jäsennys merkkijonosta"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Päivämäärän jäsentäminen merkkijonosta on yleinen tehtävä, kun käsitellään käyttäjän syötettä tai luetaan tietoja tiedostoista. Tämä sisältää merkkijonotiedon muuntamisen ohjelmointikielen tunnistamaan päivämäärämuotoon. Rustissa tämä on olennaista päivämäärien käsittelyä varten, kuten vertailuja, aritmeettisia toimintoja tai muotoilua varten, ja se parantaa tietojen validointia ja eheyttä sovelluksissa.

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
