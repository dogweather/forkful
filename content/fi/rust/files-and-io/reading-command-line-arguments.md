---
date: 2024-01-20 17:56:51.570171-07:00
description: "Komennon rivilt\xE4 argumenttien lukeminen mahdollistaa k\xE4ytt\xE4\
  j\xE4n sy\xF6tt\xE4m\xE4n tiedon hakemisen ohjelmallesi. K\xE4yt\xE4mme sit\xE4\
  , koska haluamme ohjelman toimivan\u2026"
lastmod: '2024-03-13T22:44:56.372670-06:00'
model: gpt-4-1106-preview
summary: "Komennon rivilt\xE4 argumenttien lukeminen mahdollistaa k\xE4ytt\xE4j\xE4\
  n sy\xF6tt\xE4m\xE4n tiedon hakemisen ohjelmallesi. K\xE4yt\xE4mme sit\xE4, koska\
  \ haluamme ohjelman toimivan\u2026"
title: Komennoriviparametrien lukeminen
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Komennon riviltä argumenttien lukeminen mahdollistaa käyttäjän syöttämän tiedon hakemisen ohjelmallesi. Käytämme sitä, koska haluamme ohjelman toimivan joustavasti eri tilanteissa ja käyttäjän tarpeiden mukaan.

## How to: (Kuinka tehdä:)
```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 {
        println!("Ensimmäinen argumentti on: {}", args[1]);
    } else {
        println!("Argumentteja ei annettu.");
    }
}
```
Käynnistä ohjelma komennolla `cargo run` seuraten argumentteja. Esimerkkilähtö:

`$ cargo run terve`
```
Ensimmäinen argumentti on: terve
```

`$ cargo run`
```
Argumentteja ei annettu.
```

## Deep Dive (Syvä sukellus)
Alkuaikoina komentoriviohjelmat olivat käytön perusta. Rustissa `std::env` moduuli tekee argumenttien käsittelystä helppoa. Vaihtoehtoisia tapoja ovat mm. `getopts`, `clap`, tai `structopt` kirjastot, jotka tarjoavat enemmän ominaisuuksia kuten automaattista ohjesivujen generointia. `env::args` luo iteraattorin, joka palauttaa argumentit `String` tyyppisinä, ja `collect` metodi kerää ne vektoriksi.

## See Also (Katso myös)
- Rustin standardikirjaston dokumentaatio `std::env`: https://doc.rust-lang.org/std/env/
- `clap` kirjastoa käsittelevä ohjeistus: https://clap.rs/
- `structopt` Crate-dokumentaatio: https://docs.rs/structopt/*/structopt/
