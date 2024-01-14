---
title:    "Rust: Alimerkkijonojen erottaminen"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Substringien erottaminen on tärkeä osa ohjelmointia, koska se mahdollistaa merkkijonojen käsittelyn ja analysoinnin tarkemmin. Rustin sisäänrakennetulla substring-toiminnolla on myös monia hyödyllisiä sovelluksia, kuten tekstin hakeminen ja muotoileminen.

## Miten

Rustissa on sisäänrakennettu `substring`-funktio, joka pystyy erottamaan halutun osan merkkijonosta. Tätä funktiota kutsutaan seuraavasti:

```Rust
let str = "Tämä on esimerkkiteksti";
let substring = &str[5..11];
println!("{}", substring); // tulostaa "on esi"
```

Ensimmäisessä rivissä määritellään muuttuja `str`, joka sisältää halutun merkkijonon. Toisessa rivissä luodaan uusi muuttuja `substring`, joka saa arvoksi vanhan merkkijonon tietyn osan käyttäen `substring`-funktiota. Kolmannessa rivissä tulostetaan uusi merkkijono `substring` käyttäen `println`-funktiota.

## Syvällisempi sukellus

Rustin `substring`-toiminnolla on paljon muunnostyökaluja, jotka mahdollistavat tarkemman substringien erottamisen. Esimerkiksi voit määrittää `substring`-toiminnolle hakusanan, jonka avulla voit erottaa tietyn osan merkkijonosta. Voit myös käyttää `range`-operaattoria erottaaksesi merkkijonosta haluamasi osan indeksien avulla.

```
let str = "Tämä on esimerkkiteksti";
let substring = &str["on".."mi"];
println!("{}", substring); // tulostaa "on esi"
```

Voit myös käyttää `substring`-toiminnon suodattimia, kuten `matches`, joka mahdollistaa tietyn merkkijonon löytämisen ja erottamisen. Kaikkia `substring`-toiminnon ominaisuuksia voit tutkia Rustin virallisesta dokumentaatiosta.

## Katso myös
- [Rustin virallinen dokumentaatio](https://doc.rust-lang.org/std/primitive.str.html#method.substring)
- [Rustin substring-toiminnon opas](https://www.javaer101.com/en/article/14993447.html)
- [Substringien käyttö esimerkiksi tekstin käsittelyssä](https://techmikael.com/2016/09/substrings-string-manipulation-in-rust/)