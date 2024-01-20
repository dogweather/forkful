---
title:                "HTML:n jäsentäminen"
date:                  2024-01-20T15:31:38.648491-07:00
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
HTML-tulkitseminen tarkoittaa HTML-dokumentin rakenteen muuntamista joksikin hyödynnettävämmäksi formaatiksi. Ohjelmoijat tekevät tätä, jotta voivat lukea, muokata tai uuttaa tietoja verkkosivuilta.

## How to:
Gleam ei vielä sisällä vakio HTML-parseria, joten käyttäkää ulkopuolista kirjastoa, kuten `gleam_html`. Tässä esimerkki sen käytöstä:

```gleam
import gleam_html

fn parse_html(input: String) -> Result(list(Node), Error) {
  gleam_html.parse(input)
}

pub fn main() {
  let html = "<!DOCTYPE html><html><body><h1>Terve Maailma!</h1></body></html>"
  let result = parse_html(html)

  case result {
    Ok(nodes) -> 
      nodes
    Error(_) -> 
      list()
  }
}
```

Esimerkkitulostus:

```gleam
[Element("html", [], [Element("body", [], [Element("h1", [], [Text("Terve Maailma!")])])])]
```

## Deep Dive
HTML:n tulkitsemisessa on kyse dokumentin puumaisesta rakenteesta. Historiallisesti tämä tehtiin usein säännöllisten lausekkeiden (regex) avulla, mutta tämä tapa on virhealtis ja rajoittunut. Nykyään käyttöön on tullut tehokkaampia ja luotettavampia kirjastoja.

`gleam_html` on kirjasto, joka antaa strukturoidun tavan käsitellä HTML-dokumentteja Gleamissa. Se käyttää yleensä purkaja-malli (parser-combinator) lähestymistapaa, joka muuttaa syötteen puurakenteeseen, missä jokainen elementti ja teksti on selkeästi määriteltynä.

Vaihtoehtoisia työkaluja löytyy myös muista ohjelmointikielistä, kuten Pythonin BeautifulSoup tai JavaScriptin Cheerio, mutta niiden käyttö Gleamissa vaatisi erillisiä sideja.

## See Also
- Gleam HTML library: https://hex.pm/packages/gleam_html
- Gleam language official documentation: https://gleam.run/
- "Robust HTML parsing and serializing with Rustler": This article dives into how to leverage Rust for parsing HTML in Elixir, which you might find inspiring for cross-language library usage. (ei suomennettu, ei oikea linkki)
- Alternatives in other languages: BeautifulSoup documentation (Python), Cheerio documentation (JavaScript)