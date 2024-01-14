---
title:    "Rust: Merkkijonon pituuden löytäminen"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Miksi

Rust on yksi suosituimmista ja nopeimmin kasvavista ohjelmointikielistä tällä hetkellä. Se on monipuolinen ja tehokas kieli, joka tarjoaa vakaan ja turvallisen kehitysympäristön. Yksi Rustin vahvuuksista on sen kyky käsitellä merkkijonoja tehokkaasti, mikä tekee siitä erinomaisen valinnan mihin tahansa ohjelmointiprojektiin.

## Kuinka

Rustissa merkkijonojen pituuden löytäminen on yksinkertaista ja intuitiivista. Voit käyttää `len()` -funktiota, joka palauttaa merkkijonon pituuden. Tässä on yksinkertainen esimerkki:

```Rust
let s = "Hei maailma!";
println!("Merkkijonon pituus on: {}", s.len());
```

Tämä koodinpätkä antaa tulosteeksi "Merkkijonon pituus on: 12", sillä "Hei maailma!" koostuu 12 merkistä. Näin voit helposti ja nopeasti löytää minkä tahansa merkkijonon pituuden Rustissa.

## Syventyvä sukellus

Rustin `len()` -funktio perustuu UTF-8 -koodaukseen, joka on Rustin oletuskoodaustapa. Tämä tarkoittaa, että jokainen merkki tallennetaan useana tavuna ja `len()` -funktio laskee näiden taulukoiden määrän, jotta se saa merkkijonon pituuden. Tämä tekee merkkijonon pituuden laskemisesta tarkempaa ja monipuolisempaa, mutta koodin suoritus saattaa olla hieman hitaampaa joissakin tapauksissa.

Rustissa on myös muita tapoja löytää merkkijonon pituus erilaisten tarpeiden mukaan. Voit esimerkiksi käyttää `bytes().len()` -funktiota, joka laskee merkkijonon pituuden tavuina, tai `chars().count()` -metodia, joka laskee merkkijonon pituuden merkkeinä. Joten voit valita parhaiten tarpeisiisi sopivan vaihtoehdon.

## Katso myös

- [Rustin virallinen oppimateriaali](https://www.rust-lang.org/learn)
- [The Rust Programming Language -kirja](https://doc.rust-lang.org/book/)
- [Rustin standardikirjasto](https://doc.rust-lang.org/std/)