---
title:    "Rust: Satunnaislukujen generointi"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit luoda satunnaisia numeroita? Monille tämä saattaa kuulostaa turhalta tehtävältä, mutta satunnaisilla numeroilla on monia käyttötarkoituksia ohjelmoinnissa. Niitä voidaan käyttää esimerkiksi simulaatioissa, peleissä, salausavaimissa ja satunnaislukutesteissä.

## Miten

Rust-ohjelmointikieli tarjoaa sisäänrakennetun crate-ominaisuuden, joka mahdollistaa satunnaislukujen generoinnin helposti ja luotettavasti. Crate on Rustin pakettimanageri, ja sen avulla voidaan ladata ja hallinnoida ulkopuolisia kirjastoja ja toiminnallisuuksia.

Aloitetaan luomalla uusi Rust-projekti ja lisäämällä crate-ominaisuus `rand` tiedostoon `Cargo.toml`:

```Rust
[dependencies]
rand = "0.8.3"
```

Tämän jälkeen voimme generoida satunnaisia numeroita koodissa seuraavasti:

```Rust
use rand::prelude::*;

fn main() {
    // Generoi satunnaisluku väliltä 1-100
    let random_number = rand::thread_rng().gen_range(1, 101);
    println!("Satunnaisluku: {}", random_number);
}
```

Suoritettaessa koodi tulostaa esimerkiksi seuraavanlaisen tuloksen:

```
Satunnaisluku: 42
```

Kuten näkyy, crate-ominaisuuden avulla satunnaislukujen generointi on hyvin yksinkertaista Rustissa.

## Syvällisempi sukellus

Rustin `rand` crate tarjoaa monipuolisia mahdollisuuksia satunnaislukujen generointiin. Esimerkiksi voimme generoida tietyn tyyppisiä numeroita, kuten liukulukuja tai booleeniarvoja. Lisäksi voimme määrittää oman seed-arvon, joka vaikuttaa satunnaislukujen järjestykseen.

Crate sisältää myös erilaisia algoritmeja satunnaislukujen generointiin, jotka eroavat toisistaan esimerkiksi nopeuden tai satunnaisuuden suhteen. Tämän avulla voimme valita sopivan algoritmin riippuen siitä, mikä on tarkoituksenamme generoida.

## Katso myös

- [Rustin virallinen dokumentaatio satunnaislukujen generoinnista](https://doc.rust-lang.org/std/rand/index.html)
- [`rand` crate GitHubissa](https://github.com/rust-random/rand)