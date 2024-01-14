---
title:                "Rust: Kirjoittaminen vakiovirheeseen"
simple_title:         "Kirjoittaminen vakiovirheeseen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Rust on nuori ohjelmointikieli, jossa yhdistyvät tehokkuus ja turvallisuus modernilla tavalla. Eräs tärkeä osa ohjelmointia on virheiden hallinta ja niiden raportointi. Tässä blogipostissa tarkastellaan, miten kirjoittaa standardivirheeseen Rustilla.

## Miten

Koodiesimerkit käyttävät `println!` -makroa ja `eprintln!` -makroa, jotka tulostavat merkkijonon standardilähtöön ja standardivirheeseen.

```Rust
fn main() {
    let nimi = "Iina";
    println!("Hei, minun nimeni on {}", nimi);
    eprintln!("Tämä on vakava virhe!");
}
```
Tämä koodi tulostaa standardilähtöön `Hei, minun nimeni on Iina` ja standardivirheeseen `Tämä on vakava virhe!` Huomaa, että standardivirheen tulostus eroaa tavallisesta tulostuksesta `println!`-makrolla.

## Syväsukellus

Kirjoittaessamme standardivirheeseen Rustin avulla, käytämme `std::io::stderr` -olion `write_all`-metodia. Tämä mahdollistaa myös erilaisten muuttujien ja tietorakenteiden tulostamisen standardivirheeseen.

```Rust
use std::io::Write;
fn main() {
    let virhe = "Tämä on virhe";
    let luku = 42;
    std::io::stderr().write_all(virhe.as_bytes()).unwrap();
    std::io::stderr().write_all(b" ja luvuksi arvotaan ").unwrap();
    std::io::stderr().write_all(luku.to_string().as_bytes()).unwrap();
}
```

Tämä tulostaa standardivirheeseen `Tämä on virhe ja luvuksi arvotaan 42`.

## Katso myös

- [Rust-ohjelmointikielen kotisivu](https://www.rust-lang.org/fi/)
- [Rust ohjelmointikielen opetusohjelma](https://doc.rust-lang.org/book/)
- [Rust yhteisöfoorumi](https://users.rust-lang.org/)