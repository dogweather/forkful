---
title:                "Tekstin etsiminen ja korvaaminen"
date:                  2024-01-20T17:57:40.285022-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? / Mikä ja Miksi?
Tekstissä etsiminen ja korvaaminen tarkoittaa merkkijonojen vaihtamista toisiin. Ohjelmoijat käyttävät sitä datan muokkaamiseen ja virheiden korjaamiseen.

## How to / Kuinka tehdään:
```gleam
import gleam/string

// Etsitään ja korvataan tekstiä
pub fn search_and_replace(text: String, from: String, to: String) -> String {
  string.replace(text, from, to)
}

pub fn main() {
  let original = "Terveisiä Gleamin maailmasta!"
  let corrected = search_and_replace(original, "maailmasta", "maailmankaikkeudesta")
  println(corrected)
}
```

Käynnistyksen jälkeen tulostuu: `Terveisiä Gleamin maailmankaikkeudesta!`

## Deep Dive / Syväsukellus:
Ensimmäiset tekstikorvaustyökalut ilmestyivät 1950- ja 1960-luvuilla osana tekstieditoreja. Gleamissa `string.replace` on suora tapa korvata tekstiä. Se on yksinkertaisempi kuin säännöllisiä lausekkeita käyttävät työkalut, mutta riittää moniin tarpeisiin. Säännöllisten lausekkeiden (regex) kirjastot ovat vaihtoehtoja monimutkaisempiin etsintöihin.

## See Also / Katso Myös:
- Säännöllisten lausekkeiden opas: [RegexOne](https://regexone.com/)