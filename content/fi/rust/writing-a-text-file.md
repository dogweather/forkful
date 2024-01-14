---
title:                "Rust: Tiedoston kirjoittaminen"
simple_title:         "Tiedoston kirjoittaminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kirjoittaisit tekstitiedoston Rust-ohjelmointikielellä? Rust on moderni, tehokas ja turvallinen ohjelmointikieli, joka on suunniteltu erityisesti web-sovellusten ja järjestelmäkehityksen tarpeisiin. Teksitiedostojen kirjoittaminen on olennainen osa monia ohjelmointitehtäviä, joten Rustin käyttö voi tehdä siitä mukavampaa ja luotettavampaa.

## Miten

Kirjoittaminen tekstitiedostoon Rustilla on melko yksinkertaista. Tarvitset vain muutaman Rostin perustaidon, kuten muuttujien määrittelyn ja iteraation, ja tiedostojen kirjoittamiseen liittyvät erikoistoiminnot. Alla olevassa esimerkissä luomme uuden tekstitiedoston nimeltä "teksti.txt" ja kirjoitamme siihen muutamia rivipysytyksiä.

```Rust
use std::fs::File;
use std::io::Write;

fn main() {
    // Luodaan uusi tiedosto ja tallennetaan sen viite muuttujaan.
    let mut tiedosto = File::create("teksti.txt").expect("Tiedoston luominen epäonnistui.");

    // Kirjoitetaan muutama rivi tiedostoon käyttäen write! makroa.
    write!(tiedosto, "Tervetuloa Rust-ohjelmointiin!").unwrap();
    write!(tiedosto, "Tämä on tekstirivien syöttö.").unwrap();
}
```

Suoritettaessa tätä ohjelmaa, se luo uuden tekstitiedoston ja kirjoittaa siihen kaksi riviä tekstiä. Voit tarkistaa, että tiedosto on kirjoitettu oikein avaamalla se ja lukemalla sen sisältö.

## Syventävä sukellus

Tekstitiedostojen kirjoittaminen Rustilla onnistuu myös käyttämällä "fs" -kirjaston "File::write_all" -toimintoa. Tämä voi olla hyödyllistä, jos haluat kirjoittaa isomman määrän tekstiä tai suorittaa muita tiedostoon liittyviä toimintoja. Seuraavassa esimerkissä käytetään "write_all" -toimintoa ja luodaan samantyyppinen teksti.txt-tiedosto kuin aiemmin.

```Rust
use std::fs::File;
use std::io::Write;

fn main() {
    let mut tiedosto = File::create("teksti.txt").expect("Tiedoston luominen epäonnistui.");

    tiedosto.write_all(b"Tervetuloa Rust-ohjelmointiin!\n Tämä on tekstirivien syöttö.").expect("Kirjoittaminen epäonnistui.");
}

```

Tässä esimerkissä käytetään "write_all" -toimintoa, joka ottaa vastaan tavujen (bytes) määrän ja tallentaa ne tiedostoon. Tämä auttaa myös estämään mahdollisia virheitä, jotka saattavat esiintyä tekstikoodin käytössä. Kuten aikaisemmin tarkistamme, että tiedosto on luotu ja kirjoitettu oikein avaamalla se ja tarkistamalla sen sisältö.

## Katso myös

Jos olet kiinnostunut tietämään enemmän tekstitiedostojen kirjoittamisesta Rustilla, löydät lisätietoja seuraavista linkeistä:

- [std::io::Write - Rust Documentation](https://doc.rust-lang.org/std/io/trait.Write.html)
- [The Rust Programming Language - Writing to a File](https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html#writing-to-a-file)
- [fs - Rust Documentation](https://doc.rust-lang.org/std/fs/index.html)