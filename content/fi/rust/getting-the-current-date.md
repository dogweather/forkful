---
title:                "Rust: Tämänhetkisen päivämäärän hakeminen"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi 

Jokaisen ohjelmointikielen ensimmäinen tehtävä on yleensä yksinkertainen: tulostaa nykyinen päivämäärä ja aika. Se auttaa vahvistamaan, että ohjelma toimii oikein. Rustilla on helppo tehtävä tämä tehtävä, mutta ensin sinulla täytyy olla ymmärrys siitä, miksi tätä tehtävää ylipäätään suoritetaan.

## Kuinka 

```Rust
use std::time::{SystemTime, UNIX_EPOCH};

let now = SystemTime::now(); // Hakee nykyisen ajan
let since_the_epoch = now.duration_since(UNIX_EPOCH).expect("Aika meni taaksepäin");

println!("Nykyinen aika sekunteina: {}", since_the_epoch.as_secs()); // Tulostaa sekuntien määrän
println!("Nykyinen Unix-aika: {}", since_the_epoch.as_secs() as i64); // Tulostaa UNIX-aikaa
```

Lainaus ```use```-lauseesta alussa mahdollistaa aikamallien käytön. Sitten ```now```-muuttuja heijastaa nykyistä aikaa. Datan käsittelyn jälkeen käytetään ```println!```-komentoa tulostamaan tietoa. Lopuksi, tätä tehtävää varten tarvitset tietoa UNIX-aikaan konversion avulla.

## Syväsukellus 

Nyt kun tiedät miten saadaan nykyinen aika ja miten se muunnetaan UNIX-aikaksi, on hyödyllistä ymmärtää, miten tämä prosessi toimii tarkemmin. Rust-ei tarjoa sisäistä päivämäärä- ja aika-tyyppiä, joten se käyttää ```SystemTime```-luokkaa edustamaan aikaa alustan riippumattomalla tavalla. Tämä luokka tarjoaa myös metodeja, kuten ```now()```, joka mahdollistaa ajankohtien käyttämisen järjestelmärajapinnan kautta.

Asioiden ollessa hieman monimutkaisempia, Rust tarjoaa myös ```UNIX_EPOCH```-vakion, joka edustaa Unix-ajan alkamista (tammikuu 1, 1970 kello 00:00). Tätä vakioa käytetään ```duration_since()```-metodilla, joka laskee kuluneen ajan ```UNIX_EPOCH```-ajan ja nykyisen ajan välillä.

## Katso myös

- [Rustin virallinen dokumentaatio päivämäärän ja ajan käsittelystä] (https://doc.rust-lang.org/std/time/struct.SystemTime.html)
- [UNIX-aika selitettynä kunniasta] (https://www.digitalocean.com/community/tutorials/a-concise-introduction-to-unix-time)
- [Rustopopus vuodenaikoja varten] (https://github.com/chronotope/chrono)