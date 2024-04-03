---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:50.456938-07:00
description: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hakeminen Rustissa on yleinen teht\xE4\
  v\xE4 lokitusta, aikaan perustuvia operaatioita tai yksinkertaisesti p\xE4iv\xE4\
  m\xE4\xE4r\xE4n n\xE4ytt\xE4mist\xE4 varten.\u2026"
lastmod: '2024-03-13T22:44:56.367330-06:00'
model: gpt-4-0125-preview
summary: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hakeminen Rustissa on yleinen teht\xE4\
  v\xE4 lokitusta, aikaan perustuvia operaatioita tai yksinkertaisesti p\xE4iv\xE4\
  m\xE4\xE4r\xE4n n\xE4ytt\xE4mist\xE4 varten."
title: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hankkiminen"
weight: 29
---

## Kuinka:


### Käyttäen Rustin standardikirjastoa
Rustin standardikirjasto tarjoaa rajoitetun, mutta nopean tavan saada nykyinen aika, vaikkakaan ei suoraan nykyistä päivämäärää kalenterimuodossa. Näin teet sen:

```rust
use std::time::{SystemTime, UNIX_EPOCH};

fn main() {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(n) => println!("Nykyinen aika: {} sekuntia Unix Epochista.", n.as_secs()),
        Err(_) => panic!("SystemTime ennen Unix Epochia!"),
    }
}
```

Tuloste:
```
Nykyinen aika: 1615390665 sekuntia Unix Epochista.
```

### Käyttäen Chrono-kirjastoa
Lisää kattavaa päivämäärän ja ajan toiminnallisuutta, mukaan lukien nykyisen päivämäärän saamisen, varten sinun tulisi käyttää `chrono`-kirjastoa. Lisää ensin `chrono` `Cargo.toml`-tiedostoosi:

```toml
[dependencies]
chrono = "0.4"
```

Sitten voit käyttää `chrono`-kirjastoa saadaksesi nykyisen päivämäärän:

```rust
extern crate chrono;
use chrono::{Local, Datelike};

fn main() {
    let now = Local::now();
    println!("Nykyinen päivämäärä: {}-{}-{}", now.year(), now.month(), now.day());
}
```

Tuloste:
```
Nykyinen päivämäärä: 2023-4-20
```

`Chrono`-kirjasto tekee päivämäärien ja aikojen käsittelystä yksinkertaista, tarjoten laajan valikoiman toiminnallisuuksia nykyisen päivämäärän hakemisen lisäksi, mukaan lukien päivämäärien ja aikojen jäsentämisen, muotoilun ja aritmeettiset operaatiot.
