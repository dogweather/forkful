---
title:    "Rust: Alimerkkien erottaminen"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit hajauttaa alimalleja Rust-ohjelmoinnissa? Substringien ottaminen merkkijonoista on yleinen tehtävä, jota tarvitaan usein esimerkiksi tekstianalyysissä tai tekstinpätkien manipuloinnissa. Rust tarjoaa tehokkaita ja turvallisia tapoja suorittaa tämä tehtävä ja tässä blogikirjoituksessa tutustumme tarkemmin niihin.

## Käytännössä

Rustissa on useita tapoja purkaa alimerkkejä merkkijonoista. Yksi vaihtoehto on käyttää `substring` -luokkaa, joka mahdollistaa merkkijonon jakamisen haluttuihin osiin. Esimerkiksi alla olevassa koodiesimerkissämme jaamme merkkijonon "Tervetuloa!" kahteen osaan sen perusteella, mihin haluamme sen jakaa.

```Rust
let tervehdys = "Tervetuloa!";
let (terve, nimi) = tervehdys.split_at(6);

println!("{} {} kaikki!", terve, nimi);
```

Tulostus:

```Rust
Tervetuloa! kaikki!
```

Voimme myös käyttää `substring` -luokkaa luomaan uuden merkkijonon osista toisesta merkkijonosta. Seuraavassa esimerkissä otamme ensimmäisen osan merkkijonosta, johon liitämme siihen loppuosan toisesta merkkijonosta.

```Rust
let etunimi = "John";
let sukunimi = "Smith";

let koko_nimi = etunimi.to_string() + sukunimi;

println!("Nimeni on {}", koko_nimi);
```

Tulostus:

```Rust
Nimeni on JohnSmith
```

## Syväsukellus

Rustissa on myös muita tapoja käsitellä alimainoksia, kuten `slice` -tyyppi, joka perustuu viittausten käsitteeseen. Tämä mahdollistaa alimainosten käsittelyn ilman tarvetta luoda uusia merkkijonoja. Voit lukea lisää tästä ominaisuudesta Rustin virallisesta dokumentaatiosta.

## Katso myös

- Rustin virallinen dokumentaatio substringien käsittelystä: https://doc.rust-lang.org/std/primitive.str.html#method.split_at
- Rustin virallinen dokumentaatio `slice` -tyypistä: https://doc.rust-lang.org/std/str/struct.Substr.html