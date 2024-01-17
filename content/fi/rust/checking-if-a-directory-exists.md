---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Rust: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tarkista, onko hakemisto olemassa, on yksinkertainen tapa tarkistaa, onko jokin tiedosto- tai hakemistopolku olemassa oleva vai ei. Ohjelmoijat tekevät tämän varmistaakseen, että heidän ohjelmansa suorittavat oikeat toiminnot ja välttääkseen virheitä, jotka voivat johtua puuttuvista tiedostoista tai hakemistoista.

## Kuinka?

```Rust
use std::path::Path;

if Path::new("polku/hakemisto").exists() {
    println!("Hakemisto on olemassa.");
} else {
    println!("Hakemistoa ei ole olemassa.");
}
```
Esimerkissä käytämme `std::path` -kirjastoa ja sen `exists()` -metodia tarkistaaksemme, onko `polku/hakemisto` -hakemisto olemassa. Riippuen tuloksesta, tulostamme vastaavan viestin.

## Syvä sukellus

Tarkistamalla, onko hakemisto olemassa, voidaan välttää monia virheitä, kuten tiedostojen lukemisen yrityksiä ei-olemassa olevista hakemistoista. On myös mahdollista käyttää muita vaihtoehtoja, kuten `std::fs::metadata()` -metodia, joka palauttaa tiedoston metatiedot, mukaan lukien tiedostotyypin.

## Katso myös

- [std::path::Path] (https://doc.rust-lang.org/std/path/struct.Path.html)
- [std::fs::metadata()] (https://doc.rust-lang.org/std/fs/fn.metadata.html)