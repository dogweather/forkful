---
title:                "Tekstitiedoston lukeminen"
date:                  2024-01-20T17:55:13.622010-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/reading-a-text-file.md"
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