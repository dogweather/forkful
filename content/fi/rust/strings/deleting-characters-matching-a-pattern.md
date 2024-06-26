---
date: 2024-01-20 17:43:13.101917-07:00
description: "How to: (Kuinka tehd\xE4:) Esimerkin tuloste."
lastmod: '2024-04-05T21:53:57.899887-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) Esimerkin tuloste."
title: Merkkien poistaminen hakemalla osumia kaavaan
weight: 5
---

## How to: (Kuinka tehdä:)
```Rust
fn main() {
    let original = "Ruoste - Rust 1.58.0";
    let pattern = "-";
    let result = original.replace(pattern, "");
    println!("Ennen: {}", original);
    println!("Jälkeen: {}", result);
}
```

Esimerkin tuloste:
```
Ennen: Ruoste - Rust 1.58.0
Jälkeen: Ruoste  Rust 1.58.0
```

Poistetaan kaikki numerot regex-kirjaston avulla:
```Rust
use regex::Regex;

fn main() {
    let original = "Ruoste 1.58.0";
    let regex = Regex::new(r"\d").unwrap();
    let result = regex.replace_all(&original, "");
    println!("Numeroita ilman: {}", result);
}
```

Tuloste:
```
Numeroita ilman: Ruoste ..
```

## Deep Dive (Syväkatsaus)
Merkkijonoista kuvioiden mukaan poistaminen on peräisin tiedonkäsittelytaiteesta, jossa merkkijonojen käsittely on keskeistä. Rustissa tämä tapahtuu `.replace()` tai `.replace_all()`-metodeilla. Regex-kirjastolla voimme käyttää monimutkaisempia säännöllisiä lausekkeita, joka vaatii hiukan enemmän suorituskykyä. Tämän vaihtoehdon etuna on joustavuus: voit poistaa merkkijonosta lähes mitä tahansa kuvioita.

## See Also (Katso myös)
- [Rust Documentation on `str`](https://doc.rust-lang.org/std/primitive.str.html)
- [Regex crate documentation](https://docs.rs/regex)
- [Rust by Example on Text Processing](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
