---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
html_title:           "Rust: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi ihmiset haluavat muuttaa stringin ensimmäisen kirjaimen isoksi? Tämä voi olla tarpeen esimerkiksi, jos halutaan noudattaa tiettyä nimeämiskäytäntöä tai korostaa tiettyä sanaa lauseessa.

## Miten

Capitalizing stringin Rustissa on helppoa! Ensimmäinen askel on tuoda käyttöön standardikirjaston `String` moduli, jotta voimme käyttää sen metodeja. Sitten käytämme `to_uppercase` -metodia muuttaaksemme stringin ensimmäisen kirjaimen isoksi. Tässä yksinkertainen esimerkki:

```Rust
use std::string::String;

let s = String::from("hello rust");
let capitalized = s.to_uppercase();

println!("{}", capitalized); // Tulostaa "HELLO RUST"
```

## Syvällinen sukellus

Vaikka `to_uppercase` on kätevä tapa muuttaa stringin ensimmäinen kirjain isoksi, on tärkeää huomata, että tämä metodi ei tule muuttamaan stringin alkuperäistä arvoa. Sen sijaan se palauttaa uuden `String` -olion, joka sisältää muutetun version.

Lisäksi, jos haluat muuttaa vain yksittäisen stringin kirjaimen isoksi, voit käyttää `to_uppercase` metodia suoraan merkin `char` kanssa.

```Rust
let c = 'a';
let capitalized_char = c.to_uppercase();

println!("{}", capitalized_char); // Tulostaa "A"
```

## Katso myös

- [Rust Standard Library](https://doc.rust-lang.org/std/string/index.html)
- [to_uppercase - Dokumentointi](https://doc.rust-lang.org/std/string/struct.String.html#method.to_uppercase)