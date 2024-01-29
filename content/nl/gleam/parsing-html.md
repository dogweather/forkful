---
title:                "HTML Parsen"
date:                  2024-01-28T22:03:32.618042-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML Parsen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
HTML parseren betekent het omzetten van HTML-strings naar gestructureerde gegevens. Programmeurs doen dit om informatie van webpagina's te manipuleren of te extraheren, of om veilig HTML te genereren op basis van gebruikersinvoer.

## Hoe:
Gleam heeft geen ingebouwde HTML-parserbibliotheek, maar je kunt Erlang-bibliotheken gebruiken via interop. Hier is een basisvoorbeeld met het `meeseeks`-pakket, een HTML/XML-parser:

Voeg eerst `meeseeks` toe aan je `rebar.config` deps, zoals dit:

```erlang
{deps, [
    {meeseeks, "0.15.0"}
]}.
```

Zo zou je HTML kunnen parseren en zoeken in Gleam, ervan uitgaande dat je Erlang interop correct hebt afgehandeld:

```gleam
import gleam/erlang
import meeseeks/html
import meeseeks/css

pub fn parse_and_find() -> Result(String, Nil) {
  let html = "<html><body><h1>Hallo, Gleam!</h1></body></html>"
  let doc = html |> html.parse
  let selector = css.parse("h1").unwrap()
  
  doc
  |> meeseeks.all(selector)
  |> meeseeks.text
  |> Result.map(lists.head)
}
```
Deze functie parseert de HTML, zoekt vervolgens naar `h1`-tags en krijgt de tekst. Dit is wat het uitvoeren ervan zou kunnen uitvoeren:

```shell
> parse_and_find()
Ok("Hallo, Gleam!")
```

## Diepgaand Onderzoek
Historisch gezien betekende HTML parseren in een nieuwe taal het schrijven van een eigen parser of het omhullen van een bestaande. Alternatieven zijn het gebruik van regex (doorgaans een slecht idee vanwege de complexiteit van HTML) of robuuste bibliotheken zoals `meeseeks` gebaseerd op bewezen parsers (zoals `html5ever` van Rust in het geval van `meeseeks`).

Het implementeren van HTML-parsing kan lastig zijn omdat HTML vaak niet goed gevormd of voorspelbaar is. Bibliotheken pakken dit aan door gegevens te saneren en te normaliseren. Interfacing met Erlang-bibliotheken vanuit Gleam is eenvoudig dankzij de compatibiliteit van het Erlang-ecosysteem, waardoor toegang wordt geboden tot volwassen bibliotheken zonder het wiel opnieuw te hoeven uitvinden.

## Zie Ook
Voor verder lezen en bronnen, zie:

- Meeseeks bibliotheek op Hex: https://hex.pm/packages/meeseeks
- De `html5ever` Rust-parser: https://github.com/servo/html5ever
- Erlang-interoperabiliteitsgids voor Gleam: https://gleam.run/book/tour/erlang-interop/
