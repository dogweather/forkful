---
title:                "Tietokoneohjelmoinnin artikkeli: Kirjoittaminen standardivirheeseen"
html_title:           "Rust: Tietokoneohjelmoinnin artikkeli: Kirjoittaminen standardivirheeseen"
simple_title:         "Tietokoneohjelmoinnin artikkeli: Kirjoittaminen standardivirheeseen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi kirjoittaa virhesyötteeseen?

Kun kehität Rust-ohjelmia, saatat huomata joutuvasi käsittelemään erilaisia virheitä ja poikkeustilanteita. Kirjoittaminen standardivirhesyötteeseen (standard error) auttaa sinua havaitsemaan ja käsittelemään näitä virheitä nopeasti ja tehokkaasti.

## Näin kirjoitat standardivirhesyötteeseen

```Rust
use std::io::{stderr, Write};

fn main() {
    if let Err(error) = perform_some_action() {
        writeln!(stderr(), "Virhe: {}", error).expect("Kirjoittaminen epäonnistui");
    }
}
```

#### Esimerkki tulosteesta:

```
Virhe: Tiedostoa ei löydetty
```

Standardivirhesyötteen kirjoittamiseen käytetään `writeln!`-makroa, joka ottaa parametreinaan virhesyötteen `stderr()` ja halutun viestin. Tämän jälkeen viesti kirjoitetaan standardivirhesyötteeseen. Huomaa myös `expect`-funktio, joka auttaa ilmoittamaan mahdollisista virheistä kirjoituksen yhteydessä.

## Syvempi sukellus

Rust-ohjelmointikieli tunnetaan turvallisuudestaan, ja tämä pätee myös virhesyötteen kirjoittamiseen. Rustin standardikirjasto tarjoaa erilaisia työkaluja, kuten `stderr()` ja `writeln!`-makro, joilla voidaan minimoida mahdolliset turvallisuusongelmat virheiden käsittelyssä.

Lisäksi, kirjoittaessasi virhesyötteeseen voit hyödyntää hieman erilaisia tekniikoita, kuten loggaamista (logging) ja ehdollista kirjoittamista, jotka voivat tehdä virheiden käsittelystä vieläkin kätevämpää ja tehokkaampaa. Voit myös käyttää `panic!`-makroa, joka lopettaa ohjelman suorittamisen ja kirjoittaa virhesyötteen viestin samalla kertaa.

## Katso myös

- [Rustin standardikirjasto - `std::io` (englanniksi)](https://doc.rust-lang.org/std/io/index.html)
- [Virhesyötteen hallinta Rustissa (englanniksi)](https://www.geeksforgeeks.org/error-handling-in-rust/)