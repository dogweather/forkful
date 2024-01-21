---
title:                "Merkkijonojen osien poimiminen"
date:                  2024-01-20T17:45:41.408041-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonojen osien poimiminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Substrings ovat juuri sitä, miltä ne kuulostavat: stringien aliosia. Ohjelmoijat kaivavat niitä selvittääkseen tietoa tai prosessoidakseen tekstin osia tehokkaammin.

## How to:
```gleam
import gleam/string

fn main() {
  let story = "Kaikki on parempaa saunassa"
  let sauna = string.slice(to: 24, from: 20, story)
  println(sauna) // "saunassa"
}
```

Output:
```
saunassa
```

```gleam
fn get_first_word(text: String) -> String {
  string.take_while(fn(c) { c != ' ' }, text) 
}

fn main() {
  let greeting = "Terveisiä Gleamista!"
  println(get_first_word(greeting)) // "Terveisiä"
}
```

Output:
```
Terveisiä
```

## Deep Dive
Substringien kaivelu liittyy usein datan käsittelyyn ja tekstin analysointiin. Menetelmät ovat kehittyneet vuosien saatossa, ja nykyään ne ovat vakiona useimmissa ohjelmointikielissä, Gleam mukaan lukien. Vaihtoehtoja slice:lle ovat esimerkiksi split tai regex, mutta ne ovat usein hitaampia tai monimutkaisempia käyttää. Gleamissa substringien käsittelyn tehokkuus perustuu Erlangin Beam-koneen optimointeihin ja binääritietotyyppien käyttöön.

## See Also
- Gleam's official string handling documentation: [Gleam string docs](https://gleam.run/book/tour/strings.html)