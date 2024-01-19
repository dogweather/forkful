---
title:                "Merkkijonon muuttaminen pieniksi kirjaimiksi"
html_title:           "Gleam: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Muunnetaan merkkijono pienaakkosiksi tarkoittaa, että jokainen isoaakkonen merkkijonossa muunnetaan vastaavaksi pienaakkoseksi. Tämä on hyödyllistä erityisesti vertailutilanteissa, joissa halutaan ohittaa kirjaintyyppien aiheuttamat erot: esim. hakukoneet tai käyttäjän syötteen käsittely.

## Kuinka:

```Rust
fn main() {
    let s = "Moi SUOMI!";
    println!("{}", s.to_lowercase());
}
```

Tässä ohjelman tuloste on "moi suomi!".

## Syväsukellus

1. Historiallinen yhteys: Merkkijonojen pienennys on ollut tarpeellista ohjelmoinnin alkuaikojen jälkeen, kun tietokoneet alkoivat käsitellä tekstiä. 
2. Vaihtoehdot: Rust tarjoaa muitakin tapoja työskennellä merkkijonojen kanssa, kuten `to_uppercase()` isoaakkosten luomiseksi.
3. Toteutus: Metodi `to_lowercase()` Rustissa tekee kopion merkkijonosta ja muuntaa kaikki isoaakkoset pienaakkosiksi. Huomaa: tämä ottaa huomioon myös ei-latinalaiset merkit!

## Katso myös:

[Rust-Ohjekirja: Merkkijonot](https://doc.rust-lang.org/book/ch08-02-strings.html)
[Rust-Standardikirjasto: to_lowercase()](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)