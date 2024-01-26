---
title:                "Nykyisen päivämäärän hankkiminen"
date:                  2024-01-20T15:16:17.921374-07:00
html_title:           "Bash: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? / Mitä & Miksi?
Nykyisen päivämäärän hakeminen tarkoittaa järjestelmän kalenterista päivämäärätiedon saamista. Ohjelmoijat tarvitsevat tätä toimintoa raportointiin, aikaleimojen luomiseen ja päivämääräriippuvaisten toimintojen hallintaan.

## How to: / Kuinka:
```Rust
use chrono::{DateTime, Local};

fn main() {
    let local_date: DateTime<Local> = Local::now();
    println!("Nykyinen päivämäärä: {}", local_date.format("%Y-%m-%d").to_string());
}
```

Esimerkin tulostus:
```
Nykyinen päivämäärä: 2023-04-14
```

## Deep Dive / Syväsukellus
Rustin `chrono`-kirjasto on yleinen valinta ajan käsittelyyn. Historiallisesti `std::time` ja `std::date` tarjosivat rajallisemmat työkalut. `chrono` tarjoaa laajat ominaisuudet ja helppokäyttöisen API:n. Vaihtoehtoiset kirjastot, kuten `time`, ovat myös käytössä, mutta `chrono` on suosituin.

`Local::now()` kutsu palauttaa paikallisen ajan `DateTime`-objektina, joka sisältää sekä päivämäärän että kellonajan. `format`-metodi muuntaa tämän merkkijonoksi halutussa muodossa.

## See Also / Katso Myös
- Rust `chrono` kirjaston dokumentaatio: https://docs.rs/chrono
- 'The Rust Programming Language' -kirjan luku ajan käsittelystä: https://doc.rust-lang.org/book/ch02-00-guessing-game-tutorial.html
- Rust 'time' kirjaston dokumentaatio: https://docs.rs/time
