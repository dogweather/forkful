---
date: 2024-01-20 17:56:51.570171-07:00
description: "How to: (Kuinka tehd\xE4:) K\xE4ynnist\xE4 ohjelma komennolla `cargo\
  \ run` seuraten argumentteja. Esimerkkil\xE4ht\xF6: `$ cargo run terve`."
lastmod: '2024-04-05T21:53:57.931906-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) K\xE4ynnist\xE4 ohjelma komennolla `cargo run` seuraten\
  \ argumentteja."
title: Komennoriviparametrien lukeminen
weight: 23
---

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
