---
title:                "Satunnaisten numeroiden luominen"
html_title:           "Bash: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Arpajaisnumeroiden luominen on prosessi, jossa tuotamme ennalta arvaamattomia numeroita käyttäen satunnaisgeneraattoria. Ohjelmoijat käyttävät tätä prosessia tarjotakseen dynaamisia ja ennalta arvaamattomia tuloksia ohjelmissaan.

## Miten:

Näin voit tuottaa satunnainen kokonaisluvun Rust-ohjelmalla:

```Rust
use rand::Rng;

let mut rng = rand::thread_rng();
let n: u32 = rng.gen_range(0, 10);
println!("Satunnainen luku on: {}", n);
```

Käynnistäessäsi ohjelma tuottaa satunnaisen kokonaisluvun väliltä 0-9.

## Syvällisempi katsaus:

Aikaisemmin, ennen tietokoneita, satunnaislukujen luominen ei ollut niin yksinkertaista. Se vaati monimutkaisia toimintoja, kuten erilaisten fyysisten ilmiöiden, kuten radonin hajoamisen, käyttämistä satunnaislukujen luomiseksi.

Rustissa, `rand::Rng` ja `rand::thread_rng()` ovat Rustin rand-kirjaston osia, jotka tarjoavat satunnaislukuominaisuudet.  `rand::Rng` on yhteensopiva trait, ja `rand::thread_rng()` on funktio, joka palauttaa viitteen Thread-local -satunnaisgeneraattoriin.

Vaihtoehtoina rust-lang:n rand-kirjastolle on saatavilla muutamia, kuten fastrand ja nanorand, mutta nämä eivät ole yhtä yleisesti käytettyjä tai suosittuja.

## Katso myös:

Lisätietoja satunnaisluvun tuottamisesta ja Rust-rand- kirjastosta:

- Rust’s Random Number Library: [Lukijaopas](https://rust-lang.github.io/book/ch02-00-guessing-game-tutorial.html)
- Rand-kirjaston [Käyttöopas](https://docs.rs/rand/0.8.3/rand/)
- Vaihtoehtoja rust-lang:in rand-kirjastolle [fastrand](https://crates.io/crates/fastrand) ja [nanorand](https://crates.io/crates/nanorand)