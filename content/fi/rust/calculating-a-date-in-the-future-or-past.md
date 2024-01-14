---
title:    "Rust: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Suunnittellessamme tulevia tapahtumia tai tehdessämme aikatauluja, voimme usein joutua laskemaan tietyn päivämäärän tulevaisuuteen tai menneisyyteen. Tässä blogikirjoituksessa tutustumme siihen, miten Rust-ohjelmointikielellä voimme helposti laskea päivämääriä tulevaisuuteen tai menneisyyteen.

## Miten tehdä se

Rustin `chrono` -kirjasto tarjoaa meille helpon ja tarkan tavan käsitellä päivämääriä ja aikoja. Voimme käyttää `chrono` -kirjastoa laskemaan tietyn määrän päiviä tulevaisuuteen tai menneisyyteen.

```Rust
use chrono::{Utc, Duration};

fn laske_paivamaara(pv: i64) {
    let nykyinen_paivamaara = Utc::now();
    let uusi_paivamaara = nykyinen_paivamaara + Duration::days(pv);

    println!("Päivämäärä {} päivän päästä on: {}", pv, uusi_paivamaara.format("%d.%m.%Y"));
}

fn main() {
    laske_paivamaara(14);
}
```

Tämä yksinkertainen esimerkki koodi laskee päivämäärän 14 päivää nykyisestä päivämäärästä eteenpäin ja tulostaa sen muodossa dd.mm.yyyy. Voimme käyttää samaa logiikkaa myös menneisyyteen laskemisessa vaihtamalla `Duration` -metodin `days()` `neljännesvuodeksi()`.

Voimme myös lukea päivämäärän käyttäjän syöttämästä pvm-muodostimesta ja tulostaa sen jälkeen halutun päivämäärän tulevaisuudessa tai menneisyydessä.

```Rust
use chrono::{NaiveDate, Duration};

fn laske_paivamaara(paivamaara: &str, pv: i64) {
    let parsed_date = NaiveDate::parse_from_str(paivamaara, "%Y-%m-%d").unwrap();
    let uusi_paivamaara = parsed_date + Duration::days(pv);

    println!("Päivämäärä {} päivän päästä on: {}", pv, uusi_paivamaara.format("%d.%m.%Y"));
}

fn main() {
    laske_paivamaara("2021-09-10", 30);
}
```

Tässä esimerkissä laskemme päivämäärän 30 päivää eteenpäin antamalla käyttäjän syöttää päivämäärän `YYYY-MM-DD` -muodossa.

## Syvempi sukellus

`chrono` -kirjaston lisäksi voimme myös käyttää `time` -kirjastoa laskemaan päivämääriä tulevaisuuteen tai menneisyyteen. Tämä kirjasto tarjoaa vaihtoehtoisia tapoja käsitellä ja muokata aikoja ja päivämääriä.

Voimme myös lisätä tai vähentää päivämääriä käyttämällä `date` kirjaston `add()` ja `sub()` metodeita sekä antamalla halutun ajanjakson parametriksi.

```Rust
use time::{Date, Duration};

fn laske_paivamaara(pv: i64) {
    let nykyinen_paivamaara = Date::today();
    let uusi_paivamaara = nykyinen_paivamaara.add(Duration::days(pv));

    println!("Päivämäärä {} päivän päästä on: {}", pv, uusi_paivamaara.format("%d.%m.%Y"));
}

fn main() {
    laske_paivamaara(14);
}
```

`date` -kirjaston käyttö antaa meille enemmän joustavuutta muokata päivämääriä haluamallamme tavalla.

##