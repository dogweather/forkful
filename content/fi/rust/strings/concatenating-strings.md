---
date: 2024-01-20 17:35:45.450160-07:00
description: "How to (Miten) Konkatenointi on ollut keskeinen toiminto ohjelmissa\
  \ alusta alkaen. Rustissa merkkijonojen yhdist\xE4mist\xE4 hoidetaan useilla tavoilla\
  \ riippuen\u2026"
lastmod: '2024-04-05T21:53:57.907841-06:00'
model: gpt-4-1106-preview
summary: "Rustissa merkkijonojen yhdist\xE4mist\xE4 hoidetaan useilla tavoilla riippuen\
  \ siit\xE4, tarvitseeko muuttumattomuutta vai tehokkuutta."
title: "Merkkijonojen yhdist\xE4minen"
weight: 3
---

## How to (Miten)
```Rust
fn main() {
    // Yksinkertainen konkatenointi käyttäen `+` operaattoria
    let tervehdys = "Hei ".to_string();
    let maailma = "maailma!";
    let kokonainen_viesti = tervehdys + maailma;
    println!("{}", kokonainen_viesti); // Tulostuu: Hei maailma!

    // Konkatenointi usealla merkkijonolla `format!` makrolla
    let nimi = "Pekka";
    let viesti = format!("{} {}", kokonainen_viesti, nimi);
    println!("{}", viesti); // Tulostuu: Hei maailma! Pekka
}
```

## Deep Dive (Syväsukellus)
Konkatenointi on ollut keskeinen toiminto ohjelmissa alusta alkaen. Rustissa merkkijonojen yhdistämistä hoidetaan useilla tavoilla riippuen siitä, tarvitseeko muuttumattomuutta vai tehokkuutta. `+` operaattori ottaa omistajuuden ensimmäisestä merkkijonosta ja liittää toisen siihen, mitä voisi kuvailla "paikan päällä" konkatenointina. Varomaton käyttö voi aiheuttaa suorituskyvyn ongelmia isojen tai monien merkkijonojen kohdalla. `format!` makro on joustava ja ottaa useita merkkijonoja anonyymeinä parametreinä, luoden uuden `String` olion tulokseksi.

## See Also (Katso Myös)
- Rust Book merkkijonoista: [The Rust Programming Language - Strings](https://doc.rust-lang.org/book/ch08-02-strings.html)
- Rust By Example konkatenoinnista: [Rust By Example - Strings](https://doc.rust-lang.org/rust-by-example/std/str.html)
- Rust Standard Library `String` tyyppi: [std::string::String](https://doc.rust-lang.org/std/string/struct.String.html)
