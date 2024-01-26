---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Bash: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regex on kuvioiden vertailuun käytetty syntaksi. Ohjelmoijat käyttävät sitä tekstin hakuun, korvaamiseen ja validoimiseen nopeasti.

## How to:

```gleam
import gleam/regex

fn main() {
  let pattern = regex.compile("moi").unwrap()
  let result = regex.find(pattern, "Hei, moi, maailma!")

  case result {
    Ok(matches) => io.print(matches), // Tulostaa: ["moi"]
    Error(_) => io.print("Kuvion haku epäonnistui.")
  }
}
```

## Deep Dive
Regexpit ovat UNIXista peräisin. Nykyään monet kielet, kuten JavaScript, Python ja Gleam käyttävät niitä. Vaihtoehtoina ovat tekstihaun funktiot, kuten `string.contains`. Gleam käyttää Erlangin regex-kirjastoa, mikä tekee regexp-käsittelystä tehokasta.

## See Also
- RegExr, for building and testing regex patterns: [https://regexr.com/](https://regexr.com/)
- Interactive Gleam Playground: [https://gleam.run/](https://gleam.run/)
