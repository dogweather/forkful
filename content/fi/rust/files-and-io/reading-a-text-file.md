---
date: 2024-01-20 17:55:13.622010-07:00
description: "Miten: Aiemmin ohjelmoijat k\xE4yttiv\xE4t alhaisemman tason kieliss\xE4\
  \ monimutkaista koodia tiedostojen lukuun. Rust tekee t\xE4st\xE4 helpompaa tarjoamalla\
  \ vahvoja\u2026"
lastmod: '2024-04-05T21:53:57.934023-06:00'
model: gpt-4-1106-preview
summary: "Aiemmin ohjelmoijat k\xE4yttiv\xE4t alhaisemman tason kieliss\xE4 monimutkaista\
  \ koodia tiedostojen lukuun."
title: Tekstitiedoston lukeminen
weight: 22
---

## Miten:
```Rust
use std::fs::File;
use std::io::{self, Read};

fn main() -> io::Result<()> {
    let mut file = File::open("tervehdys.txt")?; // Avaa tiedoston
    let mut content = String::new();
    file.read_to_string(&mut content)?; // Lukee tiedoston sisällön
    println!("Tiedoston sisältö:\n{}", content); // Tulostaa sisällön
    Ok(())
}
```
Tuloste:
```
Tiedoston sisältö:
Hei, Rust-ohjelmoijat!
```

## Syväsukellus
Aiemmin ohjelmoijat käyttivät alhaisemman tason kielissä monimutkaista koodia tiedostojen lukuun. Rust tekee tästä helpompaa tarjoamalla vahvoja abstraktioita, jotka piilottavat monimutkaisuuden ja hallitsevat virheet turvallisesti. Alternatiivisia tapoja tiedoston lukemiseen ovat mm. `std::io::BufRead`-traitin käyttö suorituskyvyn parantamiseksi suurilla tiedostoilla ja `std::fs::read_to_string` funktion käyttö tiedoston lukemiseen yhdellä rivillä. Toteutustiedoissa on hyvä muistaa käsitellä `Result` huolellisesti, ettei virhekäsittely jää huomiotta.

## Näe Myös
- Rust-lang: [std::fs](https://doc.rust-lang.org/std/fs/index.html)
- Rust-lang: [std::io](https://doc.rust-lang.org/std/io/index.html)
- Rust By Example: [File I/O](https://doc.rust-lang.org/rust-by-example/std_misc/file.html)
