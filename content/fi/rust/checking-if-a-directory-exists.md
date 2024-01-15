---
title:                "Tarkistetaan, onko hakemisto olemassa."
html_title:           "Rust: Tarkistetaan, onko hakemisto olemassa."
simple_title:         "Tarkistetaan, onko hakemisto olemassa."
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmassa on tarve tarkistaa, onko tietty kansio olemassa ennen kuin suoritetaan sen sisältäviä toimintoja. Tämä voi auttaa välttämään virheitä ja mahdollistaa ohjelman toiminnan joustavuuden.

## Miten

Tässä on kaksi esimerkkiä siitä, miten voit tarkistaa, onko kansio olemassa Rustilla.

```Rust
use std::fs;

let path = std::path::Path::new("polku/kansioon");

// Tarkistetaan onko kansio olemassa käyttäen fs-makroa
if fs::metadata(path).is_ok() {
    println!("Kansio on olemassa!");
}

// Tarkistetaan onko kansio olemassa käyttäen std:n metodeita
if path.exists() {
    println!("Kansio on olemassa!");
}
```

Esimerkkien tulos:

```sh
Kansio on olemassa!
```

## Syvempi sukellus

Kun tarkistat, onko kansio olemassa Rustilla, käytät todennäköisesti std-kirjaston fs- tai path-moduuleja. Näissä moduuleissa on erilaisia metodeita ja makroja, joilla voit tarkistaa tiedostojen ja kansioiden olemassaolon. Voit myös käyttää std-kirjaston error-käsittelyä, jotta ohjelma ei kaadu, jos kansioa ei löydy tai kyseessä on jokin muu virhe.

## Katso myös

- [std::fs dokumentaatio](https://doc.rust-lang.org/std/fs/)
- [std::path dokumentaatio](https://doc.rust-lang.org/std/path/)
- [std::io error-käsittely dokumentaatio](https://doc.rust-lang.org/std/io/trait.Error.html)