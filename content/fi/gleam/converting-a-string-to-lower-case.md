---
title:                "Gleam: Muunna merkkijono pienaakkosiksi"
simple_title:         "Muunna merkkijono pienaakkosiksi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi?

On hyödyllistä joskus muuttaa merkkijono pieniksi kirjaimiksi, esimerkiksi jos haluat vertailla kahta merkkijonoa ja haluat että kirjainkoko ei vaikuta vertailuun. Tämä on erityisen tärkeää suomenkielellä, jossa on paljon erikoismerkkejä ja aakkosia.

## Kuinka?

```Gleam
fn main() {
  let s = "Tämä On Merkkijono"
  let lower_case = String.to_lower_case(s)
  io.println(lower_case)
}
```

Tulostus: ```tämä on merkkijono```

## Syvällinen sukellus

Gleamilla on valmiina funktio `String.to_lower_case`, joka ottaa parametrina merkkijonon ja palauttaa uuden merkkijonon, jossa kaikki kirjaimet on muutettu pieniksi kirjaimiksi. Tämä on kätevä tapa välttää kirjainkoosta johtuvia ongelmia, kun käsitellään merkkijonoja.

Gleamin merkkijonot ovat myös Unicode-yhteensopivia, mikä tarkoittaa sitä että funktio `String.to_lower_case` osaa käsitellä myös erikoismerkkejä ja eri kielten aakkosia oikein.

## Katso myös

- [Gleam Document