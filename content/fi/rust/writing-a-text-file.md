---
title:                "Tekstitiedoston kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Kirjoittaessamme tekstitiedosto, tallennamme dataa levylle pysyvässä muodossa. Ohjelmoijat tekevät tämän datan säilyttämiseksi, jakamiseksi tai jatkokäsittelemiseksi.

## How to:
Kirjoita tekstiä tiedostoon Rustissa:
```Rust
use std::fs::File;
use std::io::Write;

fn main() -> std::io::Result<()> {
    let mut file = File::create("viesti.txt")?;
    file.write_all(b"Hello, maailma!")?;
    Ok(())
}
```
Tämä luo tiedoston `viesti.txt` ja kirjoittaa siihen "Hello, maailma!".

## Deep Dive
Tiedostojen käsittely on ollut ohjelmoinnin ytimessä 1950-luvulta lähtien. Rustissa `std::fs` ja `std::io` ovat modernit kirjastot tiedostojen käsittelyyn. Vaihtoehtoisesti voi käyttää kolmannen osapuolen kirjastoja, kuten `serde` datan serialisointiin. Rust käsittelee tiedoston kirjoittamisen suoraviivaisesti, mutta turvallisesti, käsitellen esimerkiksi virheet Result-tyypin avulla.

## See Also
- Rust-tiedostojen käsittelyn virallinen dokumentaatio: https://doc.rust-lang.org/std/fs/index.html
- `Write` trait dokumentaatio: https://doc.rust-lang.org/std/io/trait.Write.html
- Serde-kirjaston kotisivu: https://serde.rs/
