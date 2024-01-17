---
title:                "Merkkijonojen liittäminen"
html_title:           "Rust: Merkkijonojen liittäminen"
simple_title:         "Merkkijonojen liittäminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Mikä sitten on stringien yhdistäminen? Se on yksinkertaisesti kahden tai useamman merkkijonon "liittämistä" yhdeksi isoksi merkkijonoksi. Tämä on hyödyllistä ohjelmoinnissa, kun halutaan esimerkiksi tulostaa tekstiä, joka koostuu useista eri merkkijonoista.

## Kuinka?

Esimerkiksi Rustilla stringien yhdistäminen tapahtuu käyttämällä operaattoria `+` tai käyttämällä `format!` makroa. Alla on esimerkkikoodi ja sen tuloste:

```Rust
let hello = "Hei";
let name = "Maija";

let greeting = hello + " " + name;
println!("{}", greeting); // Tulostaa "Hei Maija"

let greeting2 = format!("{} {}", hello, name);
println!("{}", greeting2); // Tulostaa "Hei Maija"
```

## Syväsukellus

Stringien yhdistäminen on ollut osa ohjelmointia jo pitkään ja löytyy useista eri ohjelmointikielistä. Lisäksi on olemassa myös muita tapoja yhdistää merkkijonoja, kuten käyttämällä `StringBuilder` tai `StringBuffer` -luokkia, mutta Rustin sisäänrakennetut metodit ovat yleensä suorituskykyisempiä.

## Katso myös

Rustin virallinen dokumentaatio stringien käsittelystä: https://doc.rust-lang.org/std/string/

Mahdollisuus tehdä merkkijonoista omia tietotyyppejä Rustilla: https://doc.rust-lang.org/book/ch05-00-structs.html