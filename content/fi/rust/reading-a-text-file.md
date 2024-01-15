---
title:                "Tiedoston lukeminen"
html_title:           "Rust: Tiedoston lukeminen"
simple_title:         "Tiedoston lukeminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi
Tämä artikkeli tarjoaa perustiedot tekstitiedoston lukemisesta Rust-kielellä ja voi olla hyödyllistä niille, jotka haluavat oppia käyttämään Rustia tiedostojen käsittelyyn.

## Kuinka
Rustissa on sisäänrakennettu `std::fs` -kirjasto, joka tarjoaa useita toimintoja tiedostojen käsittelyyn. Yksi näistä toiminnoista on `read_to_string` -metodi, joka lukee tiedoston sisällön ja palauttaa sen merkkijonona.

```
Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut file = File::open("tekstitiedosto.txt").expect("Tiedostoa ei löytynyt");
    let mut content = String::new();

    file.read_to_string(&mut content).expect("Tiedoston lukeminen epäonnistui");
    println!("{}", content); // tulostaa tiedoston sisällön konsoliin
}
```
Tässä esimerkissä avaamme `tekstitiedosto.txt` -tiedoston `File` -luokalla ja tallennamme sen sisällön `content` -muuttujaan. Sitten `read_to_string` -metodilla luemme tiedoston sisällön ja tulostamme sen konsoliin.
```
Tekstitiedosto sisältö:
Tämä on esimerkkiteksti.
```

## Syvällisempi sukellus
Voit myös käyttää `BufReader`-luokkaa lukemaan tiedosto rivi kerrallaan, mikä voi olla hyödyllistä suurempien tiedostojen käsittelyssä.

```
Rust
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    let file = File::open("tekstitiedosto.txt").expect("Tiedostoa ei löytynyt");
    let reader = BufReader::new(file);

    for line in reader.lines() {
        println!("{}", line.unwrap()); // tulostaa jokaisen rivin konsoliin
    }
}
```
Tässä esimerkissä käytämme `BufReader`-luokkaa lukemaan tiedostoa ja käymme läpi jokaisen rivin `for`-silmukassa ja tulostamme sen konsoliin.

## Katso myös
- [Rustin dokumentaatio tiedostojen käsittelystä](https://doc.rust-lang.org/std/fs/index.html)
- [Rust Belt Rust -tapahtuman esitys tiedostojen käsittelystä](https://www.youtube.com/watch?v=TWC5JGF2mOM)