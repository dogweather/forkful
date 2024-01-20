---
title:                "Alimerkkijonojen poiminta"
html_title:           "Gleam: Alimerkkijonojen poiminta"
simple_title:         "Alimerkkijonojen poiminta"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Merkkijonojen alijonot ovat osa merkkijonoa, joka koostuu peräkkäisistä merkeistä. Ohjelmoijat käyttävät tätä toimintaa data-analyysiin, validointiin ja manipulointiin.

## Kuinka:

Gleamissa (versio 0.14.1) voit saada merkkijonojen alijonot kuin tämä:

```Gleam
import gleam/string

fn substring_example() {
  let my_string = "Moi, Gleam!"
  // Hae alijono indeksien 0 ja 3 väliltä
  let greeting = string.slice(my_string, 0, 3)
  let name = string.slice(my_string, 5, 10)
  (greeting, name)
}

substring_example() // Tulostus: ("Moi", "Gleam")
```

## Syvemmällä:

Merkkijonojen alijonojen etsiminen on yleinen tehtävä, joka on ollut olemassa ikuisuuden ohjelmointikielen historiassa. Gleam käyttää tuotannon tehokasta ja turvallista implementointia, strings.slice joka on suoraviivainen ja tehokas.

Vaihtoehtoisesti, voit käyttää string.split-toimintoa jakamaan merkkijono ja saamaan osia siitä. Mutta tämä on vähemmän tehoisa ja vie enemmän muistia.

```Gleam
import gleam/string

fn split_example() {
   let my_string = "Moi, Gleam!"
   string.split(my_string, ", ") // Tulostus: ["Moi", "Gleam!"]
}

split_example()
```

Tämä on yleiskatsaus Gleamin substring-toiminnallisuudesta, mutta se tarjoaa monia muita merkkijono-kirjaston menetelmiä.

## Katso myös:

Enemmän tietoa Gleamista ja sen merkkijonojen toiminnoista: 

1. Gleam's Official Guide: https://gleam.run/book/tour/strings.html
2. Gleam's API docs: https://hexdocs.pm/gleam_stdlib/gleam/string.html
3. History of Substring operations: https://en.wikipedia.org/wiki/Substring