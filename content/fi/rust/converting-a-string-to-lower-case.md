---
title:    "Rust: Merkkijonon muuttaminen pienaakkosiksi"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi: Tekstinpätkän muuntaminen pienaakkosiksi Rustilla

Useimmissa ohjelmointiprojekteissa on joskus tarve muuntaa käyttäjän antama syöte pienaakkosiksi. Tämä voi olla esimerkiksi käyttäjän kirjoittamien tietojen validointia varten tai yhtenäisen tietokannan tallennuksen varmistamiseksi. Pienaakkojen muuntaminen on yleinen tarve, ja Rust tarjoaa tehokkaita tapoja toteuttaa tämä.

## Miten: Esimerkkejä tekstinpätkän muuntamisesta pienaakkosiksi Rustilla

Rustilla on sisäänrakennettu toiminto, joka muuntaa merkkijonon pienaakkosiksi. Se käyttää Unicode-yhteensopivaa algoritmia ja tukee monenlaisia kielialueita. Alla on esimerkkejä koodista, joka käyttää tätä toimintoa:

```Rust
let name = "Matti Meikäläinen";
let name_lowercase = name.to_lowercase();
println!("{}", name_lowercase); // tulostaa "matti meikäläinen"
```

Voit myös muuntaa vain tietyn osan merkkijonosta pienaakkosiksi käyttämällä `to_lowercase`-funktioon liitettyä `get`-metodia:

```Rust
let message = "Terve! Nähdään Taas!";
let lowercase = message[..6].to_lowercase(); // muuntaa vain "Terve!" pienaakkosiksi
println!("{}", lowercase); // tulostaa "terve!"
```

Voit myös käyttää `to_lowercase`-funktiota osana `map`-korkeamman asteen toimintoa, joka käsittelee merkkijonoja vektorissa:

```Rust
let fruits = ["Omena", "Banaani", "Appelsiini"];
let lowercase_fruits = fruits
    .iter()
    .map(|x| x.to_lowercase())
    .collect::<Vec<String>>(); // muuntaa kaikki hedelmien nimet pienaakkosiksi
println!("{:?}", lowercase_fruits); // tulostaa ["omena", "banaani", "appelsiini"]
```

## Syväsukellus: Lisätietoa tekstinpätkän muuntamisesta pienaakkosiksi Rustilla

Rustin `to_lowercase`-funktio käyttää `UnicodeSpecialCasing`-tietorakennetta muuntaakseen merkkijonon pienaakkosiksi. Tämä mahdollistaa myös erikoismerkkien, kuten Å ja Ä, muuntamisen oikein. Funktiota voidaan myös kutsua käyttämällä `chars`-iteraattoria, jolloin se käsittelee merkki kerrallaan ja mahdollistaa monimutkaisemmat muunnosprosessit.

## Katso myös

- [Rustin virallinen dokumentaatio tekstinpätkän muuntamisesta pienaakkosiksi](https://doc.rust-lang.org/std/string/trait.From.html#method.to_lowercase)
- [Crate `unicase`, joka tarjoaa lisämahdollisuuksia tekstin muuntamiseen eri kielialueilla](https://crates.io/crates/unicase)
- [Esimerkkejä merkkijonon käsittelystä Rustilla](https://learnxinyminutes.com/docs/rust/)