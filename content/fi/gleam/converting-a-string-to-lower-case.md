---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
date:                  2024-01-20T17:38:34.682991-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
Muunnetaan merkkijono pieniksi kirjaimiksi. Se yhdenmukaistaa käyttäjäsyötteet ja vertailut.

## How to (Kuinka tehdä)
Gleamissa merkkijonon muuttaminen pienaakkosiksi on suoraviivaista. Käytä `string.lowercase` funktiota.
```gleam
import gleam/string

pub fn example_to_lowercase() {
  let greeting = "Hei Maailma!"
  string.lowercase(greeting)
}

// Output: "hei maailma!"
```

## Deep Dive (Syväsukellus)
Aikoinaan ASCII-taulukko määritti kirjainkoodeja, mutta nykyaikaiset standardit, kuten Unicode, ovat monimutkaisempia. Erilaisten kielten ja kulttuurien pienaakkoset voivat poiketa toisistaan. Gleamissa `string.lowercase` funktion käyttö perustuu Elixiriin, jossa unicode-tuki on kattava.

Yksi vaihtoehto on toteuttaa pienaakkosmuunnos manuaalisesti käymällä merkkijono läpi ja muuttamalla jokainen suuraakkonen vastaavaksi pienaakkoseksi. Tämä on kuitenkin työläs ja epäkäytännöllinen lähestymistapa, etenkin kun otetaan huomioon kirjainkoon kulttuurikohtaiset erot.

## See Also (Katso Myös)
- [Unicode Case Mapping](https://www.unicode.org/reports/tr21/tr21-5.html)
- [Elixir String Module](https://hexdocs.pm/elixir/String.html)
