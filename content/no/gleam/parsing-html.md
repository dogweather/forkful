---
title:                "Analysering av html"
html_title:           "Gleam: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor bry seg om å analysere HTML? Vel, hvis du for eksempel ønsker å ekstrahere spesifikk data fra en nettside, kan parsing av HTML være svært nyttig. Det kan også hjelpe deg med å lage automatiserte oppgaver, for eksempel å generere rapporter eller analysere data.

## Hvordan

For å analysere HTML i Gleam, kan du bruke biblioteket "html_beautify". Her er et eksempel på hvordan du kan bruke dette biblioteket:

```Gleam
import html_beautify.{parse, select}

//parse HTML from a URL
let html = parse("https://www.example.com")

//select specific elements using CSS selectors
let links = select(html, "a")

//print out the links
for link in links {
  io.println(link)
}
```
Dette vil gi deg en liste over alle lenkene på nettsiden som er hentet fra URLen.

## Dypdykk

For å gå enda dypere i parsing av HTML, kan du også bruke funksjonen "parse_sanitize" som fjerner all HTML formatering fra teksten og returnerer en ren tekststreng. Dette kan være nyttig hvis du kun er interessert i å analysere teksten på en nettside og ikke det visuelle innholdet.

Det er også mulig å bruke CSS-selektorer til å hente ut spesifikke elementer fra en HTML-side. Dette gjør det enklere å ekstrahere data fra en nettside uten å måtte gå gjennom alt manuelt.

## Se også

- [html_beautify dokumentasjon](https://hexdocs.pm/gleam/0.13.0/Html.Beautify.html)
- [Gleam offisiell nettside](https://gleam.run/)
- [CSS-selektorer for mer informasjon om CSS-selektorer](https://www.w3schools.com/cssref/css_selectors.asp)