---
title:                "Alastringien erotus"
html_title:           "Rust: Alastringien erotus"
simple_title:         "Alastringien erotus"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Substringien ottaminen (extracting substrings) tarkoittaa tietyn osajonon tai merkkijonon ottamista toisesta merkkijonosta. Tätä tehdään yleisesti siksi, että tietynlaiset tiedot tai merkinnät voidaan erottaa ja käsitellä erikseen.

## Kuinka:
Esimerkiksi jos haluat ottaa vain nimen sähköpostiosoitteesta, voit käyttää "split()" funktiota ja määritellä erotinmerkin, jonka mukaan jaat merkkijonon kahteen osaan. Tämän jälkeen voit ottaa halutun osan käyttämällä "get()" funktiota. 

```Rust 
let email = "esimerkki@gmail.com";
let split_email: Vec<&str> = email.split("@").collect();
let name = split_email.get(0).unwrap();
println!("Nimi: {}", name); // Tulostaa: Nimi: esimerkki 
```

## Syvempi sukellus:
Substringien ottaminen on yleinen tehtävä ohjelmoinnissa erityisesti silloin, kun halutaan käsitellä tai analysoida tietynlaisia merkkijonoja. Tämän avulla voidaan helposti erotella tiettyjä tietoja, kuten nimet, osoitteet tai puhelinnumerot, ja käsitellä niitä erikseen.

Toinen tapa ottaa substringeja on käyttää "slice" syntaksia, joka ottaa tietyn alueen merkkijonosta. Tämä on hyödyllistä esimerkiksi silloin, kun halutaan ottaa tietyn määrän merkkejä merkkijonon alusta tai lopusta.

Substringien ottaminen on myös mahdollista muilla ohjelmointikielillä, kuten Pythonissa ja Javassa. Näissä kielissä käytetään usein "substring" tai "slice" funktioita.

## Katso myös:
- [Rust `String` dokumentaatio](https://doc.rust-lang.org/std/string/struct.String.html)
- [Merkkijonon jakaminen (split() function) dokumentaatio](https://doc.rust-lang.org/std/primitive.str.html#method.split)
- [Substringien ottaminen muiden ohjelmointikielien kanssa](https://www.geeksforgeeks.org/substring-string-python/)