---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Haskell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Rust - Nykyisen päivämäärän saaminen

## Mitä & Miksi?

Nykyisen päivämäärän saaminen tarkoittaa siis, että ohjelma saa selville, mikä päivä tänään on. Se on hyödyllistä aikaleimatiedoissa, raportoinnissa, päivämäärälaskennassa ja monissa muissa tilanteissa. 

## Näin teet:

Rust-kielissä voit saada nykyisen päivämäärän `chrono`-nimisellä kirjastolla. Näin se toimii:

```Rust
use chrono::{Date, Local};

fn main() {
    let today: Date<Local> = Local::today();
    println!("Tänään on: {}", today);
}
```

Kun suoritat tämän ohjelman, saat tulosteen, joka ilmoittaa nykyisen päivämäärän, esimerkiksi "Tänään on: 2022-07-21".

## Syväluotaus

Rust-ohjelmakielessä käytetään `chrono`-kirjastoa päivämäärän ja ajan käsittelyyn. Kirjasto otettiin käyttöön vuonna 2014 ja siitä on tullut de facto työkalu ajan ja päivämäärän hoitoon Rust-kehityksessä. 

Vaihtoehtoisesti voit käyttää `time`-kirjastoa, mutta `chrono` tarjoaa usein yksinkertaisemman ja mukautuvamman API:n.

Hankkimalla nykyisen päivämäärän, `chrono`-kirjasto hankkii ensin nykyisen ajan `std::time::SystemTime`-moduulista ja muuntaa sen päivämääräksi. Tämä prosessi saattaa poiketa alustan ja käyttöjärjestelmän mukaan.

## Katso myös:

Perehdy lisää `chrono`- ja `time` -kirjastoihin seuraavista linkeistä:

- [Chrono-kirjaston dokumentaatio](https://docs.rs/chrono/0.4.19/chrono/)
- [Time-kirjaston dokumentaatio](https://docs.rs/time/0.1.42/time/)

Saadaksesi tietoa Rustin `std::time::SystemTime`-moduulista, voit katsoa tätä [SystemTime-dokumentaatiota](https://doc.rust-lang.org/std/time/struct.SystemTime.html).