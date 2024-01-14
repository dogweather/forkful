---
title:                "Rust: Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuntaa merkkijonon pieniksi kirjaimiksi? Tämä on yleinen tehtävä ohjelmoijille, jotka haluavat varmistaa, että tekstiä käsitellään yhtenäisesti ja vertailukelpoisena.

## Kuinka tehdä

[Rust](https://www.rust-lang.org/) tarjoaa helpon tavan muuntaa merkkijonon pieniksi kirjaimiksi käyttämällä `to_lowercase()` -funktiota. Se palauttaa uuden merkkijonon, joka on kopio alkuperäisestä merkkijonosta mutta pienillä kirjaimilla.

```Rust
let teksti = "Tutustu Rustiin";

let pieniksi = teksti.to_lowercase();

println!("{}", pieniksi);
// tulostaa "tutustu rustiin"
```

## Syvällisesti

Vaikka `to_lowercase()` -funktiota on helppo käyttää, on hyvä tietää mitä se tekee taustalla. Tämä funktio käyttää [Unicode Standardin](https://unicode.org/) määrittelemää käyttäytymistä merkkijonon muuntamisessa. Se varmistaa, että kaikki saatavilla olevat kielet ja kirjaimet käsitellään oikein. Lisäksi se myös käsittelee erikoismerkkejä oikein, esimerkiksi Skandinaaviset kirjaimet.

## Katso myös

- [Rustin dokumentaatio string-tyypin to_lowercase() -funktiosta](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)
- [Rust-tutoriaali: Merkkijonot](https://www.rust-lang.org/learn/strings)
- [Unicode Standardin virallinen verkkosivusto](https://unicode.org/)