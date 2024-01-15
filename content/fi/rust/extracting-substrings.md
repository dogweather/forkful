---
title:                "Alihankkeiden alaluokkien irrottaminen."
html_title:           "Rust: Alihankkeiden alaluokkien irrottaminen."
simple_title:         "Alihankkeiden alaluokkien irrottaminen."
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi haluaisit erotella alimerkkijonat Rustilla. Yleisimmät syyt ovat tekstikäsittely, tietojen etsiminen ja muotoilu. Alimerkkijonojen erottaminen on tärkeä taito, joka auttaa sinua käsittelemään merkkijonoja tehokkaammin ja luomaan monipuolisia ohjelmia.

## Miten

Alimerkkijonojen erottaminen Rustilla on helppoa ja suoraviivaista. Käytämme String-metodia nimeltä *slice*, jota voit käyttää erottamaan tietyn osan merkkijonosta. Katso esimerkki alla:

```Rust
let s = String::from("Tervetuloa maailmaan!");
let alimerkkijono = &s[3..10];

println!("{}", alimerkkijono); //tulos: vetuloa
```

Ensimmäisellä rivillä meillä on merkkijono *s*, josta haluamme erottaa alimerkkijonon. Sitten käytämme slice-metodia *[]*, jossa annamme indeksejä mistä ja mihin merkkeihin haluamme alimerkkijonon muodostuvan. Lopuksi tulostamme alimerkkijonon konsoliin.

## Syventävä sukellus

Alimerkkijonojen erottaminen Rustilla on pohjimmiltaan tapa käyttää JavaScriptin *substring()*-metodia. Erona on, että Rustin slice-metodi ei sisällytä viimeistä indeksiä alimerkkijonoon, kun taas JS:ssä se sisältyy.

```Rust
let s = String::from("Tervetuloa maailmaan!");
let alimerkkijono = &s[3..10];

println!("{}", alimerkkijono); //tulos: vetuloa
```

Tässä esimerkissä viimeinen indeksi on *9*, mutta alimerkkijonossamme on vain *7* merkkiä, jotka alkavat indeksistä *3*. Tämä siksi, että viimeinen indeksi ei sisälly alimerkkijonoon. Muista tämä, kun erotat alimerkkijonoja Rustilla.

## Katso myös

- [Rustin virallinen dokumentaatio alimerkkijonojen erottamisesta](https://doc.rust-lang.org/stable/std/primitive.str.html#method.slice)
- [W3Schools: JavaScript substring()](https://www.w3schools.com/jsref/jsref_substring.asp)