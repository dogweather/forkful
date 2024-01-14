---
title:                "Rust: Merkkijonon pituuden löytäminen"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Miksi: Merkkijonon pituuden etsiminen Rustilla

Rust on alati kasvava ja suosittu ohjelmointikieli, joka tarjoaa turvallisuutta, nopeutta ja vakautta kevyellä syntaksilla. Monet kielen ominaisuudet tekevät siitä suositun vaihtoehdon monille ohjelmoijille. Yksi näistä ominaisuuksista on sen kyky käsitellä merkkijonoja tehokkaasti. Tämän takia on tärkeää ymmärtää, kuinka Rustilla voidaan löytää merkkijonon pituus ja miksi se on hyödyllistä.

## Kuinka: Koodin esimerkkejä ja näytä tulokset käyttäen "```Rust ... ```"-koodeja

Aloittamiseksi voimme käyttää Rustin vakiofunktiota `len()` löytääksemme merkkijonon pituuden. Käyttämällä tätä funktiota, meidän tarvitsee vain antaa parametrinä haluttu merkkijono ja tuloksena saamme sen pituuden. Alla on esimerkki:

```Rust
let x = String::from("Moi maailma!");
let pituus = x.len();
println!("Merkkijonon pituus on: {}", pituus);
```

Tuloksena saamme: `Merkkijonon pituus on: 13`. Voimme myös käyttää `len()` funktiota suoraan merkkijonolle ilman, että tarvitsee tallentaa sitä muuttujaan:

```Rust
let pituus = "Tämä on vain esimerkki".len();
println!("Merkkijonon pituus on: {}", pituus);
```

Tuloksena saamme: `Merkkijonon pituus on: 22`.

## Syvällisempiä tietoja merkkijonon pituuden löytämisestä

Käyttämällä `len()` funktiota Rustissa, saamme takaisin merkkijonon pituuden tavuina. Tämä tarkoittaa, että jokainen Unicode-merkki lasketaan yhdeksi tavuksi. Mutta mitä jos haluamme laskea merkkien määrän sen sijaan, että laskemme tavujen määrän? Tässä tapauksessa voimme käyttää `chars()` funktiota, joka antaa meille merkkien määrän. Alla on esimerkki:

```Rust
let x = String::from("Hei");
let chars = x.chars().count();
println!("Merkkien määrä: {}", chars);
```

Tuloksena saamme: `Merkkien määrä: 3`. Tämä voi olla hyödyllistä, jos haluamme laskea merkkien määrän esimerkiksi käyttäjän syöttämässä merkkijonossa.

## Katso myös

- [Rustin virallinen dokumentaatio merkkijonojen käsittelystä](https://doc.rust-lang.org/std/string/index.html)
- [Rustin opetusohjelma merkkijonojen käsittelystä](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)