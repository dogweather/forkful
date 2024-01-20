---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
html_title:           "Arduino: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Kirjainkokon muuttaminen viittaa prosessiin, jossa muunnetaan kirjaimet isoiksi kirjaimiksi. Ohjelmoijat käyttävät sitä esimerkiksi käyttöliittymien yhdenmukaistamiseen ja tietojen muotoiluun.

## How to:
Seuraavassa esimerkissä käytämme Gleam-funktiota merkkijonon ensimmäisen kirjaimen isontamiseen. Esimerkissä string `moi` muuttuu `Moi`:

```gleam
import gleam/string

pub fn capitalize_first_letter(text: String) -> String {
  string.capitalize(text)
}

fn main() {
  let greeting = capitalize_first_letter("moi")
  println(greeting) // Outputs: "Moi"
}
```

## Deep Dive
Kun kapitalisointi otettiin käyttöön ohjelmoinnissa, ideana oli parantaa luettavuutta ja formaatti elintärkeitä tietoja, kuten nimiä ja otsikoita. Gleamissa, joka on moderni, turvallinen ja yhteensopiva kieli, merkkijonon kapitalisointi on yksinkertaista käyttäen standardikirjaston `string`-moduulia.

Vaihtoehtoina kapitalisoinnille ovat esimerkiksi kaikkien kirjainten isontaminen tai muuttaminen pieniksi. Toteutustiedoissa on syytä huomioida, että eri kielet ja kirjoitusjärjestelmät voivat vaikuttaa kapitalisoinnin logiikkaan.

## See Also
- Unicode standard for case mapping: [https://www.unicode.org/reports/tr21/tr21-5.html](https://www.unicode.org/reports/tr21/tr21-5.html)