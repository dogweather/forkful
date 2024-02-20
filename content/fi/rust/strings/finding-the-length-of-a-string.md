---
date: 2024-01-20 17:48:58.534358-07:00
description: "Selvitet\xE4\xE4n merkkijonon pituuden Rust-kieless\xE4: se tarkoittaa\
  \ siis merkkijonossa olevien merkkien m\xE4\xE4r\xE4n laskemista. Ohjelmoija tarvitsee\
  \ t\xE4t\xE4 tietoa\u2026"
lastmod: 2024-02-19 22:05:15.243184
model: gpt-4-1106-preview
summary: "Selvitet\xE4\xE4n merkkijonon pituuden Rust-kieless\xE4: se tarkoittaa siis\
  \ merkkijonossa olevien merkkien m\xE4\xE4r\xE4n laskemista. Ohjelmoija tarvitsee\
  \ t\xE4t\xE4 tietoa\u2026"
title: "Merkkijonon pituuden selvitt\xE4minen"
---

{{< edit_this_page >}}

## What & Why? (Mikä ja Miksi?)
Selvitetään merkkijonon pituuden Rust-kielessä: se tarkoittaa siis merkkijonossa olevien merkkien määrän laskemista. Ohjelmoija tarvitsee tätä tietoa esimerkiksi syötteiden validointiin, tekstin käsittelyyn tai muistinvarauksiin.

## How to: (Kuinka tehdä:)
```Rust
// Ensin luodaan merkkijono
let greeting = "Hei maailma!";

// Sitten lasketaan sen pituus
let length = greeting.chars().count();

println!("Merkkijonon pituus on: {}", length);
```
Sample Output:
```
Merkkijonon pituus on: 12
```

Merkkijonon `chars().count()` metodi jakaa merkkijonon yksittäisiksi merkeiksi ja laskee ne.

## Deep Dive (Syväsukellus)
Rustissa merkkijonon pituuden selvittäminen ei aina ole suoraviivaista. Merkkijonon `.len()` metodi palauttaa tavujen määrän, ei merkkien. Tämä johtuu siitä, että Rust käyttää UTF-8 -koodausta, ja merkit voivat olla eri pituisia tavuja.

Historiallisesti tämä on tärkeä ero, sillä aikaisemmin monissa kielissä oletettiin merkkien olevan aina saman pituisia. Rustissa `.chars().count()` metodi on oikea tapa saada itse merkkien määrä. Merkkijonojen `.chars()` metodi tuottaa iteraattorin, joka käy läpi merkkijonon yksittäiset Unicode-skaalauspisteet eli "char":it.

Vaihtoehtoja `.chars().count()` metodille ovat esimerkiksi `.bytes().len()`, joka antaa raakatavujen lukumäärän, ja `.graphemes(true).count()` crate-kirjaston `unicode-segmentation` avulla, joka laskee grafeemit, eli näkyvät merkkikokonaisuudet. Grafeemien laskeminen on monimutkaisempaa, mutta tarjoaa tarkemman pituudennäkymän tietyissä tilanteissa.

## See Also (Katso myös)
- Rust standardikirjaston dokumentaatio merkkijonoista: [https://doc.rust-lang.org/std/string/](https://doc.rust-lang.org/std/string/)
