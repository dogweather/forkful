---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Rust: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Regular expressions eli säännölliset lausekkeet ovat keino käsitellä merkkijonoja tavalla, joka on joustava ja monipuolinen. Ohjelmoijat käyttävät niitä usein datan käsittelemiseen, validointiin ja hakemiseen. Säännöllisten lausekkeiden avulla voit etsiä tiettyjä merkkijonon osia ja tehdä niihin muutoksia luovalla tavalla.

## Miten:
```Rust
// Esimerkki: Etsi ja korvaa
let sana = "Tänään on aurinkoinen päivä!";
let muokattu_sana = sana.replace("aurinkoinen", "sateinen");

println!("Oli sää mikä tahansa, se on aina hyvä päivä. {}", muokattu_sana);
// Tulostus: Oli sää mikä tahansa, se on aina hyvä päivä. Tänään on sateinen päivä!
```

```Rust
// Esimerkki: Tarkista sähköpostiosoite
let osoite = "esimerkki@esimerkki.com";

if osoite.contains("@") && osoite.contains(".") {
    println!("Sähköposti on kelvollinen.");
} else {
    println!("Sähköposti ei ole kelvollinen.");
}
// Tulostus: Sähköposti on kelvollinen.
```

## Suurennuslasin alla:
Säännölliset lausekkeet ovat olleet käytössä jo 1950-luvulta lähtien ja niitä käyttävät monet ohjelmointikielet, kuten Perl ja Python. Rust tarjoaa Tetchi-makroja, jotka mahdollistavat säännöllisten lausekkeiden käytön tavallisten ilmaisujen sijaan. Toisin kuin joidenkin muiden kielten tapauksessa, Rustin säännölliset lausekkeet ovat täysin kääntämisaikaisia, mikä tarkoittaa, että ne tarkistetaan jo ennen ohjelman suorittamista. Tämä auttaa havaitsemaan mahdolliset virheet jo ennen ohjelman suorittamista.

## Katso myös:
* [Rustin virallinen dokumentaatio säännöllisistä lausekkeista](https://doc.rust-lang.org/std/regex/)
* [Hyödyllisiä vinkkejä ja temppuja Rustin säännöllisiin lausekkeisiin](https://medium.com/@krishna14anu/mastering-regular-expressions-in-rust-ba32ca8b8e01)