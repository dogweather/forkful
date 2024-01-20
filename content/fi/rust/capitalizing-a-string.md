---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
html_title:           "Arduino: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Merkkijonon suurentaminen tarkoittaa tekstissä olevien kirjainten muuttamista isoiksi kirjaimiksi. Ohjelmoijat tekevät tätä datan yhdenmukaistamiseen ja ulkoasun parantamiseen.

## How to:
Katsotaan miten tämä tapahtuu Rustissa. Käytämme `to_uppercase`-metodia.

```Rust
fn main() {
    let tervehdys = "moikka maailma!";
    let isona = tervehdys.to_uppercase();

    println!("{}", isona); // output: MOIKKA MAAILMA!
}
```

## Deep Dive
Rustissa merkkijonojen suurentaminen on yleensä suoraviivaista, mutta on muutama juttu, jotka kannattaa pitää mielessä. Metodi `to_uppercase` ottaa huomioon paikalliset kirjaimiston erityispiirteet ja muuntaa kirjaimet Unicode-standardin mukaisesti. Tämä on enemmän kuin pelkkä ASCII-muunnos - se käsittelee monimutkaisempia ja eri kielten kirjainmerkkejä.

Vanhoissa ohjelmointikielissä, kuten C:ssa, merkkijonojen manipulointi oli haastavampaa ja virhealtista, kun taas Rust käsittelee merkkijonoja 'String'-oliona, joka tarjoaa turvallisen ja modernin tavan käsitellä tekstidataa.

Vaihtoehtoisesti, jos haluat vain ASCII-tekstin suurentamisen, voit käyttää `to_ascii_uppercase`-metodia, joka on nopeampi, mutta ei ymmärrä Unicode-kirjaimistoja.

## See Also
- Rust-kielen dokumentaatio `to_uppercase`-metodista: [Rust Docs: to_uppercase](https://doc.rust-lang.org/std/primitive.str.html#method.to_uppercase)
- Rust String API: [Rust String API](https://doc.rust-lang.org/std/string/struct.String.html)
- Unicode-standardin ymmärtäminen: [Unicode](http://unicode.org/standard/standard.html)