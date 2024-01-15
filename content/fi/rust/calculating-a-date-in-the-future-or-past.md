---
title:                "Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
html_title:           "Rust: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
simple_title:         "Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Usein tarvitsemme laskelmia tulevaisuuden tai menneisyyden päivämääristä esimerkiksi sovelluksissa tai projekteissa. Rust-ohjelmoinnissa on helppo ja vaivaton tapa tehdä laskelmia tällaisista päivämääristä.

## Miten

Laskelmat tulevaisuuden ja menneisyyden päivämääristä onnistuvat Rust-ohjelmoinnissa DateTime-kirjaston avulla. Voit asentaa paketin käyttämällä komentoa `cargo install chrono`.

Sitten voit aloittaa laskelmat luomalla DateTime-olion haluamallesi päivämäärälle käyttämällä `UTC` aikavyöhykkeellä ja `chrono::DateTime` rakennetta:

```Rust
extern crate chrono; // Käyttää chrono-kirjastoa

use chrono::{DateTime, Duration, Utc}; // Ottaa käyttöön tarvittavat kirjastot

let nyt = Utc::now(); // Hakee nykyisen päivämäärän ja ajan UTC-aikavyöhykkeellä
let tulevaisuudessa = nyt + Duration::weeks(2); // Lisää kaksi viikkoa nykyiseen päivämäärään
let menneisyydessä = nyt - Duration::days(5); // Vähentää viisi päivää nykyisestä päivämäärästä

println!("Päivämäärä kaksi viikkoa tulevaisuudessa on: {}", tulevaisuudessa); // Tulostaa tulevaisuuden päivämäärän
println!("Päivämäärä viisi päivää menneisyydessä oli: {}", menneisyydessä); // Tulostaa menneisyyden päivämäärän
```

Tulostus:

```
Päivämäärä kaksi viikkoa tulevaisuudessa on: 2021-09-17T22:25:49.220076800 UTC
Päivämäärä viisi päivää menneisyydessä oli: 2021-08-13T22:25:49.220076800 UTC
```

## Syväluotaus

DateTime-rakenteella on monia hyödyllisiä metodeita, joita voi käyttää päivämäärien manipulointiin. Esimerkiksi `checked_add` ja `checked_sub` metodit mahdollistavat päivämäärän lisäämisen tai vähentämisen haluamalla aikavälillä, mutta tarkistavat samalla, ettei päivämäärä ylitä vuoden tai kuukauden rajaa.

Lisäksi DateTime-rakenteella on muun muassa `format` metodi, joka mahdollistaa päivämäärän muotoilun halutun kuvion mukaan. Esimerkiksi `format("%d.%m.%Y")` tuottaa päivämäärän muodossa "13.08.2021".

Nämä ja muut DateTime-rakenteen metodit löydät [Rustin virallisesta dokumentaatiosta](https://docs.rs/chrono/0.4.19/chrono/) ja voit käyttää niitä haluamallasi tavalla päivämäärien laskemiseen tulevaisuudessa ja menneisyydessä.

## Katso myös

- Rustin virallinen dokumentaatio: https://www.rust-lang.org/
- DateTime-kirjaston dokumentaatio: https://docs.rs/chrono/0.4.19/chrono/