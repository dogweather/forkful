---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "Bash: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämäärän jäsentäminen merkkijonosta tarkoittaa merkkijonossa olevan päivämäärän muuntamista ohjelmoitavaan, käsiteltävään muotoon. Tätä tehdään, jotta päivämäärätiedot voitaisiin käsitellä ja hyödyntää tehokkaammin ohjelmassamme.

## Miten se tehdään:

Alla on koodiesimerkki siitä, miten päivämäärä jäsentää Rust-ohjelmointikielellä.

```Rust
use chrono::{DateTime, Utc, NaiveDate, NaiveDateTime};
use chrono::format::ParseError;

fn parse_date_from_string(date: &str) -> Result<NaiveDateTime, ParseError> {
    NaiveDate::parse_from_str(date, "%Y-%m-%d").map(|d| d.and_hms(0, 0, 0))
}

fn main() {
    let date = "2022-03-24";
    let parsed_date = parse_date_from_string(date).unwrap();
    println!("Parsed date: {}", parsed_date);
}
```
Koodin esimerkistä suorittaminen tulostaisi `Parsed date: 2022-03-24T00:00:00`.

## Syvällisempi sukellus:

Historiallisesti päivämäärän jäsentäminen on aina ollut tarpeellista, koska päivämäärätiedot ovat usein merkkijonomuodossa ja niitä on voitu käsitellä helpommin ohjelmoitavassa muodossa.

Vaihtoehtoisia tapoja päivämäärän jäsentämiseen Rust-ohjelmassa ovat esimerkiksi "time" ja "dateparser" -kirjastot. Valinta noiden vaihtoehtojen välillä riippuu käyttäjän vaatimuksista ja ohjelmiston tarpeesta.

Rust käyttää `chrono`-kirjastoa päivämäärän jäsentämiseen merkkijonosta. `chrono` tarjoaa joukon päivämäärän ja ajan käsittelyominaisuuksia, kuten muotoilun ja jäsentämisen.

## Tutustu myös:

Toisiin aiheeseen liittyviin lähteisiin:

- [Rust Book](https://doc.rust-lang.org/book/)
- [Chrono Crate Docs](https://docs.rs/chrono/0.4.19/chrono/)
- [Rust by Example](https://doc.rust-lang.org/rust-by-example/std_misc/chrono.html)
- [About Parsing in Rust](https://stevedonovan.github.io/rustifications/2017/09/09/parsing-in-rust.html)