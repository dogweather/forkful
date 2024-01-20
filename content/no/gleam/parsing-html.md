---
title:                "Analysering av html"
html_title:           "C#: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å parse HTML betyr at vi konverterer tekstformet HTML til strukturerte data. Dette gjør vi fordi det lar oss trekke ut nyttig informasjon og manipulere dataene.

## Hvordan Gjøre: 

La oss gi et eksempel ved å bruke `floki` pakken i Gleam.

```Gleam
import gleam/httpc
import gleam/uri.uri
import floki

fn hent_html() {
  let url = uri.parse("https://example.com").unwrap()
  let response = httpc.get(url).unwrap()
  response.body
}

fn parse_html(html: String) {
  let parsed = floki.parse_document(html).unwrap()
  parsed
}

fn main(_) {
  let html = hent_html()
  let parsed = parse_html(html)
  gleam.io.println(parsed)  
}
```

Denne koden blir brukt til å hente HTML fra en nettside og parse det. Den returnerte verdien vil være en strukturert versjon av HTML-dokumentet.

## Dyp Dykk

HTML-parsing- teknikker har utviklet seg mye gjennom årene. Fra å være en ren tekstprosessering, til å bruke komplekse biblioteker og verktøy som DOM-parser, SAX-parser og mer.

I Gleam har vi `floki`, en rask og fleksibel HTML-parser. Den konverterer HTML-tekst til en strukturert beyting. Du kan deretter filtrere, transformere eller inspisere dataene etter behov.

Alternativer til parsing inkluderer screen scraping og bruk av APIer for databehandling. Disse kan være mer effektive i bestemte situasjoner, men gir ikke samme grad av fleksibilitet eller allsidig informasjonstilgang som parsing frembringer.

## Se Også:

1. `floki` dokumentasjon: 
[https://docs.gleam.run/stdlib/floki.html](https://docs.gleam.run/stdlib/floki.html)
2. HTML Parsing teknikker:
[https://en.wikipedia.org/wiki/HTML_parsing](https://en.wikipedia.org/wiki/HTML_parsing)
3. `gleam/httpc` dokumentasjon: 
[https://docs.gleam.run/stdlib/httpc.html](https://docs.gleam.run/stdlib/httpc.html)
4. `gleam/uri` dokumentasjon: 
[https://docs.gleam.run/stdlib/uri.html](https://docs.gleam.run/stdlib/uri.html)