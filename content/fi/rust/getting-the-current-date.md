---
title:    "Rust: Päivämäärän hankkiminen tietokoneohjelmoinnissa"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi Rust on hyvä valinta päivämäärän hankkimiseen?

Päivämäärän hankkiminen on yksi yleisimpiä askelmia ohjelmoinnissa, oli sitten kyseessä sivuston päivitys tai raportin luominen. Rustilla on monia etuja, kuten suorituskyky ja turvallisuus, jotka tekevät siitä hyvän valinnan päivämäärän hankkimiseen. Lisäksi Rustin laaja yhteisö tarjoaa paljon apua ja resursseja tässä prosessissa.

## Miten hankitaan päivämäärä Rustilla?

Aloita asentamalla "chrono" kirjasto Cargo.toml tiedostoon: 
```
[dependencies]
chrono = "0.4.19"
```
Seuraavaksi tuo kirjasto käyttöön Rust koodiin: 
```Rust
use chrono::{Utc, Local, DateTime};
```
Voit nyt käyttää Utc ja Local rakenteita hankkimaan joko UTC tai paikallisen päivämäärän: 
```Rust
let current_utc = Utc::now();
let current_local = Local::now();
```
Lopuksi voit tulostaa päivämäärän halutussa muodossa: 
```Rust
println!("Järjestelmän aikavyöhykkeen UTC-aika on {}", current_utc);
println!("Paikallisaika on {}", current_local.format("%d.%m.%Y %H:%M:%S"));
```

## Syvä sukellus päivämäärän hankkimiseen Rustilla

Chrono-kirjasto tarjoaa paljon enemmän kuin vain päivämäärän hankkimisen. Voit esimerkiksi muuttaa päivämäärän haluttuun aikavyöhykkeeseen tai vertailla päivämääriä muihin aikoihin. Kirjastolla on myös käteviä toimintoja, kuten päivämäärien lisääminen ja vähentäminen. Syvemmän ymmärryksen saavuttamiseksi voit tutustua Chrono-dokumentaatioon tai liittyä Rustin yhteisöön saadaksesi apua.

## Katso myös

- Chrono-dokumentaatio: https://docs.rs/chrono/0.4.19/chrono/
- Rustin yhteisö: https://www.rust-lang.org/en-US/community.html