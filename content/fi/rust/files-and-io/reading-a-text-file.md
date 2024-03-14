---
date: 2024-01-20 17:55:13.622010-07:00
description: "Rustissa tekstitiedoston lukeminen tarkoittaa tiedoston sis\xE4ll\xF6\
  n saattamista ohjelman k\xE4ytt\xF6\xF6n. Ohjelmoijat lukevat tiedostoja, koska\
  \ niist\xE4 saadaan\u2026"
lastmod: '2024-03-13T22:44:56.374669-06:00'
model: gpt-4-1106-preview
summary: "Rustissa tekstitiedoston lukeminen tarkoittaa tiedoston sis\xE4ll\xF6n saattamista\
  \ ohjelman k\xE4ytt\xF6\xF6n. Ohjelmoijat lukevat tiedostoja, koska niist\xE4 saadaan\u2026"
title: Tekstitiedoston lukeminen
---

{{< edit_this_page >}}

## Mitä & Miksi?
Rustissa tekstitiedoston lukeminen tarkoittaa tiedoston sisällön saattamista ohjelman käyttöön. Ohjelmoijat lukevat tiedostoja, koska niistä saadaan tärkeää dataa, kuten asetuksia, käyttäjätietoja tai resursseja.

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
