---
title:    "Rust: Tekstitiedoston kirjoittaminen."
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Miksi kirjoittaa tekstitiedostoja?

Tekstiedostojen kirjoittaminen on tärkeä osa ohjelmointia monilla kielillä, mukaan lukien Rust. Ne mahdollistavat tiedon tallentamisen ja käsittelyn pysyvästi, mikä on välttämätöntä monissa sovelluksissa. Se on myös hyödyllistä silloin, kun haluat tallentaa käyttäjän syöttämät tiedot ja käyttää niitä myöhemmin.

## Kuinka kirjoittaa tekstitiedostoja Rust-ohjelmassa

Tekstitiedoston kirjoittaminen Rust-ohjelmassa on helppoa. Ensimmäiseksi on tuotava "std::fs" -kirjasto ohjelmaan. Tämän jälkeen voit käyttää "File" -luokkaa luomaan ja avaamaan tekstitiedoston. Seuraavaksi voit käyttää `write_all` -metodia kirjoittamaan tekstiä tiedostoon. Lopuksi on tärkeää sulkea tiedosto, jotta muut ohjelmat voivat käyttää sitä myöhemmin.

```
use std::fs::File;

let mut tiedosto = File::create("tiedosto.txt").expect("Tiedostoa ei voitu luoda!");
tiedosto.write_all(b"Hei maailma!")
       .expect("Tiedoston kirjoittaminen epäonnistui!");
```

Tämä koodi luo `tiedosto.txt` -nimisen tiedoston ja kirjoittaa siihen "Hei maailma!". Tiedosto sijaitsee samassa hakemistossa kuin ohjelma.

## Syvempi sukellus tekstitiedoston kirjoittamiseen

Tekstitiedostojen kirjoittaminen Rustissa on hieman erilainen kuin muissa kielissä, koska siinä käytetään `rustc_serialize` -kirjastoa muuttamaan tekstiä tavuiksi. Tämä johtuu Rustin vahvasta tyyppijärjestelmästä.

```
extern crate rustc_serialize;

use std::fs::File;
use std::io::Write;
use rustc_serialize::base64::ToBase64;

let mut tiedosto = File::create("salattu.txt").expect("Tiedostoa ei voitu luoda!");
let data = "Salainen viesti".to_string();
tiedosto.write_all(data.as_bytes().to_base64().as_bytes())
       .expect("Tiedoston kirjoittaminen epäonnistui!");
```

Tässä esimerkissä ensin tuodaan `rustc_serialize` -kirjasto ja muut tarvittavat kirjastot. Tämän jälkeen luodaan muuttuja `data`, joka sisältää salaisen viestin. Tämän jälkeen muutetaan `data` tavuiksi käyttämällä `to_base64()` -metodia ja kirjoitetaan se tiedostoon. Tämän ansiosta tiedosto sisältää salatun viestin ja sitä ei voi lukea ilman purkukoodia.

## Katso myös

- [Rustin tekstitiedoston kirjoittamisen dokumentaatio](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Rustin `rustc_serialize` -kirjaston dokumentaatio](https://doc.servo.org/rustc_serialize/)
- [Tietoja tekstitiedostojen kirjoittamisesta Rustissa (englanniksi)](https://dev.to/ladybug/working-with-text-files-in-rust-31g7)