---
title:    "Rust: Kahden päivämäärän vertailu"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi vertailla kahta päivämäärää?

Päivämäärien vertaaminen on olennainen osa ohjelmointia. Se auttaa meitä selvittämään, ovatko kaksi päivämäärää samassa ajassa, tai onko tietty päivämäärä ennen tai jälkeen toista. Vertailemalla päivämääriä, voimme myös laskea, kuinka monta päivää tai kuukautta on kulunut kahden päivämäärän välillä.

## Miten vertailla kahden päivämäärän välillä Rust-kielen avulla?

Päivämäärien vertaileminen Rust-ohjelmointikielellä on melko helppoa. Voit käyttää `chrono` -kirjastoa, joka tarjoaa käteviä apufunktioita päivämäärien vertailuun. Alla on esimerkki kahden päivämäärän vertailusta ja tulostuksesta:

```Rust
use chrono::prelude::*;

fn main() {
    let date_1 = Utc.ymd(2021, 02, 15);
    let date_2 = Utc.ymd(2021, 01, 01);

    if date_1 > date_2 {
        println!("Date 1 is after Date 2");
    } else if date_1 < date_2 {
        println!("Date 1 is before Date 2");
    } else {
        println!("Date 1 is same as Date 2");
    }
}
```

Tulostaa:

```
Date 1 is after Date 2
```

## Syvempi sukellus päivämäärien vertailuun

Päivämäärien vertaileminen perustuu niiden muuntamiseen numeraalisiksi arvoiksi ja näiden numeroiden keskinäiseen vertailuun. Päivämäärien muuntamisessa käytetään yleensä Unix-aikaleimoja, jotka ovat sekunteja, jotka ovat kuluneet 1. tammikuuta 1970 kello 00:00 UTC: sta lähtien.

`chrono` -kirjaston `DateTime` -objektit sisältävät tietoa päivämäärän ja ajan lisäksi myös aikavyöhykkeestä, mikä on tärkeää päivämäärien vertailussa. On myös tärkeää muistaa, että päivämäärien tulee olla samassa muodossa vertailun onnistumiseksi.

## Katso myös

- `chrono` -kirjaston dokumentaatio: https://docs.rs/chrono/
- Rust-kielen virallinen verkkosivusto: https://www.rust-lang.org/
- Suomen Rust-kehittäjät ry: https://www.rust-lang.fi/