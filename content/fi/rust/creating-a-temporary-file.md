---
title:                "Rust: Väliaikaisen tiedoston luominen"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Väliaikaisten tiedostojen luominen on tärkeä osa monia ohjelmointiprojekteja. Ne voivat auttaa käsittelemään tilapäistä tietoa, tallentamaan väliaikaisia tietorakenteita tai yksinkertaisesti tekemään tiedostojen käsittelystä helpompaa. Rustilla voit helposti luoda ja käsitellä väliaikaisia tiedostoja.

## Miten

Luodaksesi väliaikaisen tiedoston Rustilla, käyttöjärjestelmäsi täytyy tukea POSIX-rajapintaa. Tämä pätee yleensä kaikkiin Unix-pohjaisiin järjestelmiin, kuten Linuxiin ja MacOS:ään. Voit käyttää `std::fs::File::create()` -metodia luodaksesi tiedoston ja `tempfile::NamedTempFile::new()` -metodia luodaksesi väliaikaisen tiedoston polun.

```Rust
use std::io::prelude::*;
use std::fs::File;
use tempfile::NamedTempFile;

fn main() {
    // luodaan väliaikainen tiedosto sijaintiin "/tmp/my_temp_file"
    let mut temp_file = NamedTempFile::new("/tmp/my_temp_file").unwrap();
    
    // kirjoitetaan tiedostoon
    writeln!(temp_file, "Tämä on väliaikainen tiedosto!").unwrap();
    
    // voit käyttää väliaikaista tiedostoa kuten normaalia tiedostoa
    let mut file = File::open("/tmp/my_temp_file").unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    
    println!("{}", contents); // tulostaa "Tämä on väliaikainen tiedosto!"
}
```

Voit myös käyttää `tempfile::Builder` -luokkaa määrittääksesi tarkemmin väliaikaisen tiedoston ominaisuuksia, kuten nimen ja sijainnin.

## Syväsukellus

Kun luet ja kirjoitat väliaikaisiin tiedostoihin Rustilla, varmista, että käytät turvallisia ja luotettavia toimintoja. Voit esimerkiksi käyttää `tempfile::NamedTempFile::persist()` -metodia tallentaaksesi väliaikaisen tiedoston pysyvästi.

On myös tärkeää huomata, että väliaikaiset tiedostot poistetaan automaattisesti, kun ohjelma suljetaan tai kun `temp_file` -muuttuja tuhotaan. Voit myös manuaalisesti poistaa väliaikaisen tiedoston käyttämällä `temp_file.close()` -metodia.

## Katso myös

- [Rustin virallis dokumentaatio väliaikaisista tiedostoista](https://doc.rust-lang.org/std/fs/struct.File.html#method.create)
- [tempfile-kirjaston dokumentaatio](https://docs.rs/tempfile/3.1.0/tempfile/)
- [POSIX:n määritelmä Wikipedia:ssa](https://fi.wikipedia.org/wiki/POSIX)