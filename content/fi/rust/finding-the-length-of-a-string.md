---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Rust: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Stringin pituuden löytäminen on yksinkertainen tehtävä, jossa ohjelmoijat löytävät merkkijonon pituuden, eli kuinka monta merkkiä merkkijonossa on. Tämä on usein tarpeellista esimerkiksi merkkijonojen käsittelyssä tai laskutoimituksissa.

Ohjelmoijat tekevät tätä tehtävää löytääkseen tarvittavat tiedot merkkijonon manipuloimiseksi halutulla tavalla. Ilman merkkijonon pituuden tuntemista, ohjelmoijat eivät pystyisi luomaan tehokasta ja tarkkaa koodia, joten tämä taito on erittäin tärkeä osa ohjelmointia.

## Miten:

```Rust
let s = "Tämä on esimerkki merkkijonosta";
let length = s.len();
println!("Merkkijonon pituus on {}", length);
```
#### Output:
```
Merkkijonon pituus on 29
```

## Syventyminen:

Ohjelmoijien ei tarvitse usein miettiä merkkijonon pituuden löytämistä, sillä lähes jokaisessa ohjelmointikielessä on käytettävissä valmiina funktio, joka hoitaa tämän tehtävän. Stringin pituuden löytäminen on myös yksi tapa käsitellä merkkijonoja, mutta on myös muita tapoja, kuten esimerkiksi kääntää merkkijono taulukoksi ja laskea sen pituus.

Rustin tapauksessa merkkijonon pituuden löytämisen taustalla on käytännössä UTF-8 merkistöandardi, joka määrittelee miten merkkijonoja käsitellään ja lasketaan. Tämän ansiosta Rust pystyy käsittelemään erikoismerkkejä ja erilaisia kielialueita tarkasti.

## Katso myös:

- Rustin virallinen dokumentaatio: https://doc.rust-lang.org/std/primitive.str.html#method.len
- UTF-8 merkistöandardi: https://www.utf8-chartable.de/unicode-utf8-table.pl