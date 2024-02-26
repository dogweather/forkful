---
date: 2024-01-26 03:47:07.034314-07:00
description: "Numeroiden py\xF6rist\xE4minen tarkoittaa niiden s\xE4\xE4t\xE4mist\xE4\
  \ l\xE4himp\xE4\xE4n kokonaislukuun tai tietyn tarkkuuden murto-osaan. Ohjelmoijat\
  \ py\xF6rist\xE4v\xE4t numeroita\u2026"
lastmod: '2024-02-25T18:49:53.276815-07:00'
model: gpt-4-0125-preview
summary: "Numeroiden py\xF6rist\xE4minen tarkoittaa niiden s\xE4\xE4t\xE4mist\xE4\
  \ l\xE4himp\xE4\xE4n kokonaislukuun tai tietyn tarkkuuden murto-osaan. Ohjelmoijat\
  \ py\xF6rist\xE4v\xE4t numeroita\u2026"
title: "Numerojen py\xF6rist\xE4minen"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Numeroiden pyöristäminen tarkoittaa niiden säätämistä lähimpään kokonaislukuun tai tietyn tarkkuuden murto-osaan. Ohjelmoijat pyöristävät numeroita yksinkertaistaakseen arvoja ihmisen luettaviksi, täyttääkseen määrittelyvaatimukset tai vähentääkseen laskennallista taakkaa liukulukutoimituksissa.

## Kuinka:
Rust tekee pyöristämisestä tuulenhuuhtoman. Tutustu näihin menetelmiin `f32` tai `f64` tyypeille:

```rust
fn main() {
    let num = 2.34567;

    // Pyöristä lähimpään kokonaislukuun
    let round = num.round();
    println!("Pyöristys: {}", round); // Pyöristys: 2

    // Lattia - suurin kokonaisluku, joka on pienempi tai yhtä suuri kuin numero
    let floor = num.floor();
    println!("Lattia: {}", floor); // Lattia: 2

    // Katto - pienin kokonaisluku, joka on suurempi tai yhtä suuri kuin numero
    let ceil = num.ceil();
    println!("Katto: {}", ceil); // Katto: 3

    // Trunkkaus - kokonaislukuosa ilman desimaalilukuja
    let trunc = num.trunc();
    println!("Trunkkaus: {}", trunc); // Trunkkaus: 2

    // Lähimpään kymmenen potenssin monikertaan
    let multiple_of_ten = (num * 100.0).round() / 100.0;
    println!("Pyöristetty 2 desimaalin tarkkuuteen: {}", multiple_of_ten); // Pyöristetty 2 desimaalin tarkkuuteen: 2.35
}
```

## Syväsukellus
Historiallisesti pyöristäminen on ollut ratkaisevaa, jotta äärettömät desimaalit tai irrationaaliset numerot mahtuvat rajoitettuihin digitaalisiin tiloihin - välttämättömyys muinaisille tietokoneille, joilla oli niukasti muistia. Ajattele abakusta, mutta vähemmän nikkarointia, enemmän matikkaa.

Vaihtoehdot Rustin natiivimenetelmille sisältävät:
1. `format!` makro merkkijonojen muotoiluun, joka oletusarvoisesti pyöristää.
2. Ulkoiset pakkaukset erikoistuneisiin matemaattisiin tehtäviin, kuten `round` pakkaus tarkemmalla hallinnalla.

Rustin pyöristysoperaatiot noudattavat IEEE-standardeja - tekninen jargoni sille, että "se pyöristää kuten matikan opettajasi haluaa." Lisäksi, binääriesitysten vuoksi jotkut numerot, kuten 0.1, eivät voi perinteisesti pyöristyä niiden äärettömän binääriesityksen vuoksi.

## Katso Myös
- Rust doc primitiivityyppien menetelmistä: https://doc.rust-lang.org/std/primitive.f64.html
- IEEE-standardi liukulukuaritmetiikalle (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- "round" pakkaus monimutkaisempaan pyöristämiseen: https://crates.io/crates/round
