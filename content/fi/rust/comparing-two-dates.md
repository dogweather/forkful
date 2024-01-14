---
title:                "Rust: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi
Vertaamalla kahta päivämäärää voidaan helposti tarkistaa esimerkiksi kahden päivämäärän välillä oleva aikaero tai selvittää, kumpi päivämäärä on tulevaisuudessa tai menneisyydessä. Tässä blogikirjoituksessa käsitellään, miten tätä tehtävää voi suorittaa Rust-ohjelmointikielellä.

## Miten
Vertailuun kahta päivämäärää voidaan käyttää DateTime-tyypin muuttujia ja niiden tarjoamia metodeja. Päivämäärän voi luoda luomalla ensin uuden DateTime-instanssin käyttämällä DateTime::parse_from_str-funktiota ja antamalla sille päivämäärän merkkijonona muodossa "YYYY-MM-DD". 
```
Rust
use std::time::Duration;
use chrono::{DateTime, Utc};

let date1 = DateTime::parse_from_str("2021-01-01", "%Y-%m-%d").expect("Päivämäärän luonti epäonnistui!");
let date2 = DateTime::parse_from_str("2021-02-01", "%Y-%m-%d").expect("Päivämäärän luonti epäonnistui!");
```
Nyt voimme käyttää DateTime-muuttujien metodeja kuten .year(), .month() ja .day() saadaksemme päivämäärän osat ja vertailla niitä esimerkiksi if/else-lausekkeilla.
```
Rust
if date1 > date2 {
    println!("Ensimmäinen päivämäärä on myöhäisempi");
} else if date1 < date2 {
    println!("Toinen päivämäärä on myöhäisempi");
} else {
    println!("Päivämäärät ovat samat");
}
```
## Syvempi sukellus
DateTime-tyyppiä käytetään myös monissa muissa tilanteissa päivämäärä- ja aika-arvojen käsittelyssä Rustissa. Tärkeitä metodeja tähän tarkoitukseen ovat esimerkiksi .now(), jolla saadaan nykyinen päivämäärä ja aika, ja .format(), jolla voidaan muotoilla päivämäärä- ja aika-arvoja halutunlaisiksi merkkijonoiksi.

## Katso myös
- [DateTime-tyyppi Rustin virallisessa dokumentaatiossa](https://doc.rust-lang.org/std/time/struct.DateTime.html)
- [Rust By Example -sivusto, jossa esimerkkejä Rust-ohjelmoinnin perusteista](https://doc.rust-lang.org/std/time/struct.DateTime.html)
- [Chrono-kirjasto, joka tarjoaa laajemmat mahdollisuudet päivämäärä- ja aikamuotoiluihin Rustissa](https://crates.io/crates/chrono)