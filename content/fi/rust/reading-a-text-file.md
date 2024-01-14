---
title:                "Rust: Tiedoston lukeminen"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Monet ihmiset aloittavat ohjelmoinnin Rust-kielenä nykyään, mutta millainen on tekstintiedoston luku ja miksi se on tärkeää? Tässä blogikirjoituksessa kerromme tarkemmin siitä, miksi siihen kannattaa perehtyä.

## Miten

Koodin lukeminen tekstitiedostosta Rustilla on melko yksinkertaista ja vaivatonta. Alla esittelemme muutaman esimerkin, joiden avulla voit aloittaa.

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    // Avataan tiedosto ja luetaan sen sisältö merkkijonoksi
    let mut file = File::open("tekstitiedosto.txt").expect("Tiedoston lukeminen epäonnistui");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Tiedoston lukeminen epäonnistui");

    // Tulostetaan tiedoston sisältö
    println!("{}", contents);
}
```

### Esimerkkituloste:

```
Tämä on esimerkki tekstistä
joka on tallennettu tekstitiedostoon.
```

Voit myös lukea tiedostoa rivi kerrallaan käyttämällä `BufReader`-luokkaa:

```Rust
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    // Avataan tiedosto ja luodaan BufReader-olio
    let file = File::open("tekstitiedosto.txt").expect("Tiedoston lukeminen epäonnistui");
    let reader = BufReader::new(file);

    // Käydään läpi tiedoston rivit ja tulostetaan ne
    for line in reader.lines() {
        println!("{}", line.expect("Tiedoston lukeminen epäonnistui"));
    }
}
```

### Esimerkkituloste:

```
Tämä on esimerkki tekstistä
joka on tallennettu tekstitiedostoon.
```

## Syventävä sukellus

Tekstitiedostojen lukeminen on tärkeä osa ohjelmointia ja sitä käytetään usein esimerkiksi tietokantojen ja käyttäjän antamien syötteiden käsittelyssä. Rust tarjoaa hyödyllisiä työkaluja, kuten `File`- ja `BufReader`-luokat, jotka helpottavat tiedoston lukemista ja käsittelemistä.

On myös tärkeää huomata, että tiedoston käsittely voi joskus aiheuttaa ohjelmassa virheitä. Siksi on hyvä käyttää `expect`-metodin sijaan esimerkiksi `match`-lauseketta, jotta ohjelma osaa käsitellä mahdolliset virhetilanteet.

## Katso myös

- [Rust-ohjelmointikielen virallinen verkkosivusto](https://www.rust-lang.org/fi)
- [Rust-opetusohjelmat](https://doc.rust-lang.org/book/)
- [Tiedoston lukeminen Rustilla-ohjeet](https://doc.rust-lang.org/std/fs/struct.File.html#method.read_to_string)
- [BufReader-luokan dokumentaatio](https://doc.rust-lang.org/std/io/struct.BufReader.html)