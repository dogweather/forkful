---
title:                "Rust: Näin löydät merkkijonon pituuden"
simple_title:         "Näin löydät merkkijonon pituuden"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi: Miksi etsiä tekstin pituus?

Rust on suosittu ja suorituskykyinen ohjelmointikieli, joka tunnetaan erityisesti sen nopeudesta ja luotettavuudesta. Yksi hyödyllisimmistä ominaisuuksista Rustissa on sen tarjoama helppo tapa löytää tekstin pituus. Tämä on hyödyllistä monissa tilanteissa, esimerkiksi tiedostojen käsittelyssä tai käyttäjän syötteiden tarkistamisessa.

## Kuinka: Esimerkkejä koodista ja tulostuksista

Yksi tapa löytää tekstin pituus Rustissa on käyttää "len" -funktiota, joka palauttaa tekstin merkkien määrän. Esimerkiksi seuraava koodi näyttää, kuinka tulostaa tekstin "Hei maailma" pituus:

```Rust
fn main() {
    let teksti = "Hei maailma";
    let pituus = teksti.len();
    println!("Tekstin pituus on {}", pituus);
}
```

Tämä tulostaa:

`Tekstin pituus on 11`

Toinen asia, jota kannattaa tietää, on että tekstiä voi myös käsitellä "byte"-tasoilla. Tämä tarkoittaa, että voit käyttää "len_bytes" -funktiota, joka palauttaa tekstin pituuden tavuina. Esimerkiksi seuraava koodi näyttää, kuinka tulostaa saman tekstin pituus tavuina:

```Rust
fn main() {
    let teksti = "Hei maailma";
    let pituus = teksti.len_bytes();
    println!("Tekstin pituus on {} tavua", pituus);
}
```

Tämä tulostaa:

`Tekstin pituus on 11 tavua`

## Syväsukellus: Tietoa tekstin pituuden löytämisestä

Rustissa tekstin pituus lasketaan tavuina, ei merkkeinä, koska se käyttää Unicode-standardia. Tämä tarkoittaa, että esimerkiksi kirjainten "ä" ja "ö" pituus on kaksi tavua. Tämä on tärkeä huomioitava, jos haluat käsitellä tekstejä eri tavoilla.

Lisäksi Rustin len-funktio palauttaa "usize"-tyypin, joka kuvaa tietokoneen muistin osoittimen koon. Tämä voi aiheuttaa ongelmia suurempien tekstien käsittelyssä, sillä "usize" -tyypin maksimi on erilainen eri käyttöjärjestelmissä.

## Katso myös

- [Rustin dokumentaatio tekstin pituuden löytämisestä](https://doc.rust-lang.org/std/primitive.str.html#method.len)
- [Rust-opetusohjelma Merkkijonot](https://doc.rust-lang.org/book/ch08-02-strings.html)