---
title:                "Satunnaislukujen generointi"
date:                  2024-01-20T17:50:00.554263-07:00
model:                 gpt-4-1106-preview
simple_title:         "Satunnaislukujen generointi"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Arpajaiset syntyvät satunnaisilla numeroilla. Ohjelmoijat käyttävät niitä simulaatioissa, peleissä ja turvallisuudessa, koska tarvitaan ennalta arvaamatonta dataa.

## How to: (Kuinka Tehdä:)
Rust-kielessä käytät `rand`-kirjastoa, joka on saatavilla crates.io:ssa. Asenna `rand` lisäämällä Cargo.toml-tiedostoosi `rand` ja käytä ohessa olevaa koodiesimerkkiä:

```Rust
use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();
    let luku: u8 = rng.gen(); // Arpoo satunnaisluvun 0-255
    println!("Arvottu luku on: {}", luku);
}
```

Esimerkkituloste voisi olla:

```
Arvottu luku on: 152
```

## Deep Dive (Sukellus Syvyyksiin)
Ennen tietokoneita, arvonta tapahtui fyysisin menetelmin, kuten arpakuutiolla. Nykyään käytämme algoritmeja, kuten lineaarikongruenssia tai Mersenne Twisteriä. Rustissa `rand`-kirjasto tekee työn sinulle. Se on jakelutestattu ja suunniteltu olemaan tarpeeksi satunnainen useimpiin käyttötarkoituksiin. 

Vaihtoehtoisesti voit käyttää `rand::distributions` tarjontaa spesifimpään datan arvontaan. Suorituskyky ja turvallisuus ovat molemmat keskeisiä tekijöitä satunnaisuuden implementoinnissa.

## See Also (Katso Myös)
- Rust dokumentaatio `rand` moduulin käytöstä: [https://docs.rs/rand/](https://docs.rs/rand/)
- Rust Book, opas Rust-kielen perusteisiin: [https://doc.rust-lang.org/book/](https://doc.rust-lang.org/book/)
