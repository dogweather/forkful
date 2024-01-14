---
title:                "Rust: Tarkistetaan, onko hakemistoa olemassa"
simple_title:         "Tarkistetaan, onko hakemistoa olemassa"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi tarkistaa, onko hakemistoa olemassa?

Hakemisto on tiedostojen kokoelma, joka voi sisältää esimerkiksi ohjelmien tai dokumenttien tiedostoja. On tärkeää tietää, onko hakemistoa olemassa, jotta voidaan varmistaa, että tarvittavat tiedostot ovat saatavilla ja ohjelma voi toimia oikein.

## Kuinka tarkistaa, onko hakemisto olemassa

```Rust
use std::fs;

fn main() {
    let result = fs::metadata("hakemisto");

    match result {
        Ok(metadata) => {
            if metadata.is_dir() {
                println!("Hakemisto on olemassa!");
            } else {
                println!("Hakemistoa ei ole olemassa!");
            }
        },
        Err(_e) => println!("Hakemistoa ei ole olemassa!"),
    }
}
```

Kun käytämme `fs::metadata` -funktiota ja annamme sille hakemistopolun parametrina, se palauttaa `Ok`-arvon, jos hakemisto on olemassa. Voimme sitten käyttää `is_dir()` -metodia tarkistaaksemme, onko se todella hakemisto. Jos hakemisto ei ole olemassa, funktio palauttaa `Err`-arvon.

## Syvempi sukellus

`fs::metadata` -funktion sijaan voit myös käyttää `fs::metadata()` -funktiota, joka palauttaa `Ok`-arvon vain silloin, kun hakemisto on olemassa. Muussa tapauksessa se palauttaa virheen.

Voit myös käyttää `exists()` -metodia tarkistaaksesi, onko hakemisto olemassa ilman, että käytät `metadata()` -funktiota. Tämä metodi palauttaa totuusarvon sen perusteella, onko hakemisto olemassa vai ei.

## Katso myös

- [Rustin tiedostonhallinta](https://doc.rust-lang.org/std/fs/)
- [Hakemisto-operaatiot Rustissa](https://blog.legacyteam.info/2018/12/14/directory-operations-in-rust/)
- [Hallitse tiedostojärjestelmääsi Rustin avulla](https://medium.com/@fardjad/handle-your-filesystem-with-rust-e0e7a0b92deb)