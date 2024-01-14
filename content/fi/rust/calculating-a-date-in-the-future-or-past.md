---
title:                "Rust: Laskeminen tulevaan tai menneeseen päivämäärään"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmiston on laskettava päivämäärä tulevaisuudessa tai menneisyydessä. Tähän on monia mahdollisia syitä, kuten aikaperustaisen palkan laskeminen tai tulevien tapahtumien aikatauluttaminen. Jokainen sovellus voi tarvita tätä ominaisuutta eri syistä, mutta yhteinen tekijä on päivämäärän laskeminen halutun ajanjakson päässä nykyisestä ajasta.

## Miten

Rustilla on joukko sisäänrakennettuja työkaluja ja kirjastoja, jotka tekevät päivämäärän laskemisen helpoksi. Ensimmäinen askel on lisätä `chrono`-kirjasto `Cargo.toml` tiedostoon:

```Rust
[dependencies]
chrono = "0.4"
```

Seuraavaksi voit luoda uuden `DateTime` -muuttujan nykyisellä ajalla ja käyttää `chrono` -kirjastoa lisätäksesi tai vähentääksesi päiviä tai muita aikayksiköitä. Alla on esimerkki kahden päivän päästä olevan päivämäärän laskemisesta:

```Rust
use chrono::{DateTime, Datelike, Local, Duration};

fn main() {
   let today: DateTime<Local> = Local::now();
   let two_days_from_today: DateTime<Local> = today + Duration::days(2);

   println!("Päivämäärä kahden päivän päästä on {}.", two_days_from_today.date())
}
```

Tämä koodi tuottaa seuraavan tulosteen:

```Rust
Päivämäärä kahden päivän päästä on 2021-11-11.
```

Muita hyödyllisiä aikayksiköitä, joita voit käyttää `chrono` -kirjastossa, ovat esimerkiksi `minutes()`, `hours()`, `weeks()` jne.

## Syvällinen sukellus

Jotta voitaisiin laskea tarkempia päivämääriä, kuten palkanmaksupäivä kuukauden lopussa tai tulevien tapahtumien aikataulutus vuosien päähän, sinun kannattaa tutustua `chrono` -kirjaston dokumentaatioon ja erilaisiin aikayksiköihin, joita voit käyttää. Lisäksi voit luoda omia `Duration`-muuttujia esimerkiksi yhdistämällä erilaisia aikayksiköitä ja käyttää niitä päivämäärän laskemiseen.

## Katso myös

- [Chrono Crate Documentation](https://docs.rs/chrono/latest/chrono/)
- [Rust Language Reference](https://doc.rust-lang.org/reference/index.html)
- [Cargo Package Registry](https://crates.io/)