---
title:                "Innføring i html-analysering"
html_title:           "Gleam: Innføring i html-analysering"
simple_title:         "Innføring i html-analysering"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/parsing-html.md"
---

{{< edit_this_page >}}

---------------------------------------

## Hva & Hvorfor?
Parsing HTML er prosessen med å strukturere og tolke informasjonen som finnes i HTML-kode, som er kodespråket som brukes for å lage nettsider. Programmerere bruker parsing for å kunne manipulere eller hente ut spesifikke data fra nettsider, som for eksempel å lese og manipulere brukerinput eller å automatisk generere innhold.

## Hvordan:
Gleam har innebygde funksjoner og biblioteker for å enkelt parse HTML. Et eksempel på dette er funksjonen `Html.parse`, som leser og strukturerer HTML-koden og returnerer et tre med alle elementer og deres attributter. Her er et eksempel på bruk av denne funksjonen:

```Gleam 
let result = Html.parse("<p>Hello <b>world</b>!</p>")

Html.get_text(result) // output: "Hello world!"
Html.get_children(result) // output: [ "<p>", "<b>world</b>", "!" ]
Html.get_attributes(result) // output: { inner_text: "Hello world!" }
```
I dette eksempelet ser vi at funksjonen `Html.parse` har analysert og strukturert HTML-koden og returnert et tre med informasjonen.

## Dypdykk:
Parsing av HTML har vært en viktig del av utvikling av nettsider siden det ble introdusert på 90-tallet. Tidligere ble dette gjort manuelt, men med innføring av programmeringsspråk som Gleam har denne prosessen blitt automatisert og mer effektiv.

En alternativ måte å parse HTML på er å bruke et tredjepartsbibliotek som f.eks. `htmlparser2` for Node.js. Dette biblioteket har et større utvalg av funksjoner og muligheter, men Gleams innebygde funksjoner kan være tilstrekkelig for enklere parsing.

Ved bruk av `Html.parse` funksjonen kan man også spesifisere en liste med tillatte og uønskede elementer, noe som gir større kontroll over hva som blir returnert fra parsingen.

## Se også:
- Offisiell Gleam dokumentasjon: https://gleam.run/documentation/
- Tutorial om bruk av Gleam og parsing av HTML: https://dev.to/raphaelbaude/learn-gleam-part-4-parsing-html-with-gleam-248l