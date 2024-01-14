---
title:                "Rust: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi luoda väliaikainen tiedosto?

Monista syistä voi olla hyödyllistä luoda väliaikainen tiedosto Rust-ohjelmassasi. Yksi yleinen syy on, että haluat tallentaa väliaikaisia tietoja, kuten väliaikaisia laskelmia, jotka eivät ole pysyvästi tallennusarvoisia. Väliaikainen tiedosto voi myös auttaa ylläpitämään järjestystä ja siisteyttä ohjelman suorituksen aikana.

## Miten luoda väliaikainen tiedosto?

Luodaksesi väliaikaisen tiedoston Rust-ohjelmassa, voit käyttää "tempfile" -kirjastoa. Alla on yksinkertainen esimerkki, joka luo väliaikaisen tiedoston nimeltä "temp.txt" ja kirjoittaa siihen tekstiä:

```Rust
use std::fs::File;
use std::io::prelude::*;
use tempfile::tempfile;

fn main() {
    let mut temp_file = tempfile().unwrap();
    temp_file.write_all(b"Tämä on väliaikainen tiedosto").unwrap();
}
```

Kun suoritat tämän ohjelman, saat "temp.txt" tiedoston, joka sisältää tekstin "Tämä on väliaikainen tiedosto". Voit myös halutessasi määrittää väliaikaisen tiedoston sijainnin ja nimen.

## Syvemmälle väliaikaisen tiedoston luomiseen

"Tempfile" -kirjasto tarjoaa erilaisia ​​toimintoja väliaikaisten tiedostojen luomiseen ja käyttämiseen. Voit esimerkiksi määrittää tiedoston sijainnin ja nimen, tai voit käyttää "tempdir" -funktiota luodaksesi väliaikaisen kansion, jossa voit tallentaa useita tiedostoja. Tarkempia tietoja löydät "tempfile" dokumentaatiosta.

### Tiedostojen poistaminen

Väliaikaiset tiedostot on hyvä poistaa ohjelman suorituksen jälkeen, jotta ne eivät vie turhaan tilaa. "Tempfile" -kirjastossa on funktio "tempfile_cleanup", joka poistaa kaikki väliaikaiset tiedostot ja kansioiden oikeuksien poisto, sekä asettaa niille kansion ja tiedoston poisto käyttöoikeudet. Voit myös käyttää "NamedTempFile" -luokkaa, joka poistaa tiedoston automaattisesti, kun se poistuu sovelluksen ulkopuolella.

## Katso myös

- [Tempfile dokumentaatio](https://docs.rs/tempfile/)
- [Rust-kielen virallinen verkkosivusto](https://www.rust-lang.org/)
- [Rust-ohjelmoinnin aloittaminen](https://www.rust-lang.org/learn)