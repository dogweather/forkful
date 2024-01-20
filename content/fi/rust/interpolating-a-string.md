---
title:                "Merkkijonon interpolointi"
html_title:           "Bash: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Stringien interpolaatio Rust-ohjelmointikielessä

## Mikä & Miksi?
Stringien interpolaatio tarkoittaa muuttujien tai lausekkeiden asettamista suoraan merkkijonojen sisään. Ohjelmoijat käyttävät sitä välttääkseen pitkät ja monimutkaiset merkkijonojen yhdistelmät.

## Kuinka:
Rust-kielisissä ohjelmissa, `format!`-makroa käytetään usein stringien interpolaatioon. Katsotaanpa esimerkkiä:

```Rust
let nimi = "Matti";
let tervehdys = format!("Hei, {}!", nimi);
println!("{}", tervehdys);
```

Tämä tulostaa: `"Hei, Matti!"`.

## Syvällisempi katsaus
Stringien interpolaatiolla on pitkä historia. Se on ollut osa ohjelmointikieliä jo ennen Rustin olemassaoloa, kuten Perlissä ja PHP:ssa.

Vaihtoehtoisesti Rustissa voidaan käyttää `println!`-makroa suoraan:

```Rust
let nimi = "Matti";
println!("Hei, {}!", nimi);
```

Tämä tulostaa saman kuin aiempi esimerkki.

Rust käyttää makroja stringien interpolaatiota varten, toisin kuin jotkut muut kielet, jotka sisältävät syntaksin suoraan kielen rakenteeseen. 

Sen lisäksi että `format!` ja `println!`-makroja voidaan käyttää muuttujien arvojen sisällyttämiseen merkkijonojen sisään, ne tukevat myös monia formatointiasetuksia. Esimerkiksi, voit määrittää numeroiden tarkkuuden tai merkkijonojen leveyden.

## Katso myös
- [Rust-dokumentaatio: format!](https://doc.rust-lang.org/std/macro.format.html)
- [Rust-dokumentaatio: println!](https://doc.rust-lang.org/std/macro.println.html)
- [Rust by Example: Formatted print](https://doc.rust-lang.org/rust-by-example/hello/print.html)