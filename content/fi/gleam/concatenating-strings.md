---
title:                "Merkkijonojen yhdistäminen"
date:                  2024-01-20T17:34:58.150677-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Stringien yhdistäminen tarkoittaa kahden tai useamman tekstinpätkän liittämistä yhteen. Koodarit tekevät tätä, koska tarvitsevat usein yhdistellä tekstiä dynaamisesti esimerkiksi käyttäjän syötteen tai tiedon näyttämiseen.

## How to: (Kuinka tehdään:)
Concatenating strings in Gleam can be done using the `+` operator. Here's a quick example:

```gleam
fn main() {
  let greeting = "Hei, "
  let name = "Maija!"
  let welcome_message = greeting + name
  welcome_message
}

// Output: "Hei, Maija!"
```

You can also use the `string.concat` function for lists of strings:

```gleam
import gleam/string

fn main() {
  let phrases = ["JavaScript on ", "ihan ", "kiva."]
  let sentence = string.concat(phrases)
  sentence
}

// Output: "JavaScript on ihan kiva."
```

## Deep Dive (Syväsukellus):
Ennen stringien yhdistämistyökaluja, kehittäjien piti luoda monimutkaisia ohjelmia yhdistääkseen tekstiä, usein käyttäen matalan tason ohjelmointikieliä. Vaihtoehto stringien yhdistämiseen voi olla esimerkiksi mallinjäsennys (template parsing), missä käyttäjä syöttää rakennetta ja sisältöä yhdistetään. Tämä on vakiintuneempaa moderneissa web-sovelluksissa.

Gleamissa, joka on staattisesti tyypitetty funktionaalinen ohjelmointikieli, stringien yhdistäminen on tehty suoraviivaiseksi ja turvalliseksi. Periaatteessa, kun käytät `+` operaattoria yhdistämään stringejä, kieli tarkistaa että molemmat operandit ovat todellakin stringejä, mikä estää virheisiä tietotyyppejä yhdistymästä.

## See Also (Katso Myös):
Lisätietoja ja resursseja löydät Gleamin ohjekirjasta:

- Gleam language official website: [https://gleam.run](https://gleam.run)
- Effective String Handling and Manipulation in Gleam: Community tutorials or blog posts, search for them on tech-savvy platforms like [https://dev.to](https://dev.to)
