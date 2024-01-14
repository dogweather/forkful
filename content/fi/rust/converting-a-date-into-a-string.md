---
title:                "Rust: Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Rust-ohjelmointikieli on noussut suosituksi vaihtoehdoksi monien muiden ohjelmointikielten rinnalla, ja yksi sen suosion syistä on sen kyky käsitellä päivämäärätietoja tehokkaasti. Tässä blogikirjoituksessa keskitymme tarkastelemaan, miksi ja miten päivämäärä voidaan muuntaa merkkijonoksi Rustissa.

## Miten

Rustilla on sisäänrakennettu DateTime-kirjasto, joka mahdollistaa päivämäärätiedon käsittelemisen. Alla olevassa koodiesimerkissä näytämme, miten voit ensin luoda DateTime-olion ja sitten muuntaa sen merkkijonoksi käyttämällä format!()-makroa:

```Rust
use std::time::SystemTime;
use chrono::{DateTime, Local};

fn main() {
    let now: DateTime<Local> = DateTime::from(SystemTime::now());
    let date_string = format!("{}", now.format("%Y-%m-%d"));
    println!("{}", date_string);
}
```

Koodin tuloste on tällainen:

```
2021-07-20
```

Koodiesimerkissä käytetään `chrono`-kirjastoa, joka tarjoaa lisätoimintoja päivämäärätietojen käsittelyyn. Voit muuttaa format!()-makron avulla merkkijonon ulkoasua vaihtamalla `%Y-%m-%d`-osan haluamaasi muotoon.

## Syväsukellus

Päivämäärätiedon muuntaminen merkkijonoksi voi olla hyödyllistä esimerkiksi silloin, kun haluat tallentaa päivämäärän tietokantaan tai tallentaa sen tiedostoon. Rustin DateTime-kirjasto takaa, että päivämäärätiedon käsittely on turvallista ja tehokasta.

On myös huomionarvoista, että Rustin DateTime-kirjasto sisältää monia muita hyödyllisiä toimintoja, kuten päivämäärän lisäämisen, vähentämisen ja vertailun. Kannattaa tutustua dokumentaatioon ja löytää lisää tapoja käsitellä päivämääriä ohjelmissasi.

## Katso myös

- [Rustin virallinen DateTime-dokumentaatio](https://doc.rust-lang.org/std/time/struct.SystemTime.html)
- [Datetime kirjasto Rustilla](https://crates.io/crates/datetime)
- [Ohjeet päivämäärätietojen käsittelyyn Rustissa](https://www.codementor.io/@riccardo_cardin/how-to-work-with-dates-and-time-with-rust-t2fls6enor)