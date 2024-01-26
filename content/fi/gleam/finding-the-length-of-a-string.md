---
title:                "Merkkijonon pituuden selvittäminen"
date:                  2024-01-20T17:47:15.779227-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Stringin pituus kertoo merkkien määrän. Ohjelmoijat käyttävät sitä syötteen validoinnissa, tiedon käsittelyssä ja UI-logiikassa.

## Kuinka:
```gleam
import gleam/string

fn main() {
  let tervehdys = "Hei maailma!"
  let pituus = string.len(tervehdys)
  pituus
}
```
Tulostus: `12`

```gleam
import gleam/string

fn main() {
  "Kissa".len()
  // Tulostus: 5
}
```

## Syväluotaus
Historiallisesti, stringin pituuden laskeminen on ollut perustoiminnallisuus ohjelmointikielissä. Gleamissa `.len()`-funktio palauttaa stringin Unicode-skaalareiden lukumäärän, joka voi poiketa visuaalisten merkkien määrästä. Esimerkiksi, normaali ASCII-teksti ja emoji-merkit voivat vaikuttaa pituuden laskentaan eri tavalla. Vaihtoehtoisesti, jotkut kielet tarjoavat eri metodeja merkkijonojen pituuksien laskentaan, esimerkiksi bytemäärän tai visuaalisten merkkien määrän perusteella.

## Katso Myös
- Gleam string module: https://hexdocs.pm/gleam_stdlib/gleam/string/
- Unicode in Gleam: https://gleam.run/book/tour/unicode.html
- String handling in programming: https://en.wikipedia.org/wiki/String_(computer_science)
