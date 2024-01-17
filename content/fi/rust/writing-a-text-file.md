---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "Rust: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Tiedoston kirjoittaminen on yksinkertainen tapa tallentaa tietoa tietokoneellesi. Ohjelmoijat käyttävät tätä työkalua tallentaakseen tietoja, joita he tarvitsevat myöhemmin käyttääkseen ja lukemiseen.

## Kuinka tehdä se:
Rustilla, tiedoston kirjoittaminen on helppoa ja selkeää. Käytä ```std::fs::File``` -rajoitinta luodaksesi uuden tiedoston ja kirjoita siihen haluamasi sisältö ```write_all ()``` -toiminnon avulla. Katso seuraava koodiesimerkki nähdäksesi kuinka se tehdään:

```rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut file = File::create("uusi_tiedosto.txt")
        .expect("Ei voitu luoda uutta tiedostoa");
    
    file.write_all(b"Tervetuloa Rust-maailmaan!")
        .expect("Ei voitu kirjoittaa tiedostoon");
}
```

Tämä luo uuden tiedoston nimeltä "uusi_tiedosto.txt" ja kirjoittaa siihen tekstirivin "Tervetuloa Rust-maailmaan!". Voit tarkistaa tiedoston sisällön avaamalla sen tekstieditorilla tai käyttämällä `cat` -komentoa terminaalissa.

## Syvempi sukellus:
Tiedostonkirjoitus on yksi helpoimmista tavoista tallentaa tietoa tietokoneellesi. Ennen Rustia, C-kielessä käytettiin `FILE` -rakennetta ja `fwrite()` -funktiota tiedostojen kirjoittamiseen. Muihin vaihtoehtoihin kuuluu myös Pythonin `open()` -funktio ja Java IO -kirjasto.

Tiedostonkirjoituksen toteutuksessa käytetään `std::io` -moduulia, jota käytetään tiedostojen lukemiseen ja kirjoittamiseen. `File` -rajapinta käyttää tiedostoputkia, jotka ovat järjestelmän resursseja tiedostojen lukemiseen ja kirjoittamiseen.

## Katso myös:
- [Rustin virallinen verkkosivusto](https://www.rust-lang.org/)
- [Tiedostojen kirjoittaminen Rustilla](https://doc.rust-lang.org/std/io/trait.Write.html)
- [Rustin opas tiedostonkirjoitukseen](https://doc.rust-lang.org/book/ch12-02-reading-a-file.html)