---
title:                "Numerojen pyöristäminen"
date:                  2024-01-26T03:44:41.807210-07:00
model:                 gpt-4-0125-preview
simple_title:         "Numerojen pyöristäminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/rounding-numbers.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Numeroiden pyöristäminen tarkoittaa arvon tarkistamista lähimpään määriteltyyn paikkaan—kuten 2,56:sta 3:een, jos pyöristämme kokonaislukuihin. Ohjelmoijat tekevät tämän yksinkertaisuuden vuoksi tai tiettyjen numeeristen määritysten täyttämiseksi, yleensä välttääkseen vivahteita, joita aiheuttaa liukulukujen tarkkuusvirheet tai tehdäkseen tulosteesta käyttäjäystävällisen.

## Kuinka:
Gleamissa pyöristäminen ei ole osa vakio kirjastoa viime tarkistukseni mukaan, mutta tässä on, miten yleensä pyöristäisit liukuluvun lähimpään kokonaislukuun käyttäen suoraan Erlang-funktioita:

```gleam
external fn erlang_round(Float) -> Int = "erlang" "round"

pub fn main() {
  let rounded = erlang_round(2.56)
  rounded // Tulostaa: 3
}
```

Tuloste:
```
3
```

Onko sinulla mielessä eri tarkkuus? Siis, pyöristäminen kahteen desimaalipaikkaan? Tarvitsemme hieman matematiikkaa:

```gleam
pub fn round_to_two_places(num: Float) -> Float {
  let multiplier = 100.0
  let tmp = num * multiplier
  let round_tmp = erlang_round(tmp)
  round_tmp / multiplier
}

pub fn main() {
    round_to_two_places(2.569) // Tulostaa: 2.57
}
```

Tuloste:
```
2.57
```

## Syväsukellus
Historiallisesti numeroiden pyöristäminen on ollut ratkaisevan tärkeää, erityisesti rahoitus- ja tieteellisissä laskelmissa, joissa tarkkuus ja standardit ovat erittäin tärkeitä. Ilman pyöristämistä saisit ikäviä pitkiä desimaaleja kaikkialle, mikä tekisi laskelmista epäkäytännöllisiä ja alttiita virheille.

Ohjelmointimaailmassa eri kielet tarjoavat erilaisia lähestymistapoja, sisäänrakennetuista funktioista kattaviin matematiikkakirjastoihin. Pyöristäminen saattaa sisältää erilaisia sääntöjä – esimerkiksi "pyöristä puolikas ylös" (tavallinen menetelmä) tai "pyöristä puolikas parilliseen" (usein käytetty talouslaskelmissa välttämään vinoumaa).

Gleam, ollessaan nuori kieli, jolla on juurensa Erlangissa, nojaa Erlangin vankkaan numerofunktioiden sarjaan. Kielen kasvaessa, saatamme nähdä natiivien funktioiden käyttöönoton, vähentäen tarvetta kutsua ulkoisia rutiineja.

## Katso Myös
- Erlangin :math-moduuli lisää numeroiden käsittelyyn: https://erlang.org/doc/man/math.html
- Miksi pyöristäminen voi olla hankalaa, IEEE Liukulukustandardi: https://ieeexplore.ieee.org/document/8766229
- Kiinnostunut matematiikan takana? Tarkista "Mitä jokaisen tietojenkäsittelijän tulisi tietää liukulukuaritmetiikasta": https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html
