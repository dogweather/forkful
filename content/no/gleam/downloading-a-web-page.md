---
title:                "Laste ned en nettside"
html_title:           "Gleam: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å laste ned en nettside handler om å hente all informasjonen fra en bestemt nettside og lagre den på din egen datamaskin. Programmere gjør dette for å få tilgang til og behandle informasjonen på en enklere måte.

## Hvordan:
Gleam har innebygde funksjoner som gjør det enkelt å laste ned en nettside. Ved å bruke ```Gleam.Download.get```, kan du spesifisere en URL og få tilgang til nettsiden som en streng. Se eksempel nedenfor: 
```
let nettside = http.get("https://www.example.com/")
```
Etter å ha lastet ned nettsiden, kan du bruke ```String.split``` for å behandle informasjonen og få ut de delene du trenger, som i eksempelet nedenfor:
```
let deler = String.split("nettside", "<h1>")
```
Dette vil dele opp nettsiden basert på titlene på nettsiden og returnere en liste med relevante deler. 

## Dypdykk:
Å laste ned en nettside har vært en viktig del av webutvikling i lang tid. Før i tiden var det vanlig å bruke kompliserte teknikker for å få tilgang til og behandle informasjonen fra et nettsted, men med fremveksten av programmeringsspråk som Gleam, har det blitt mye enklere. Alternativene til å laste ned en nettside inkluderer også å bruke APIer eller webskraping-verktøy. Implementeringsdetaljer for å laste ned en nettside kan variere avhengig av programmeringsspråket du bruker, men Gleam har gjort det svært enkelt for å få tilgang til og behandle nettsider.

## Se også:
- [Gleam sin offisielle dokumentasjon](https://gleam.run/documentation/)
- [En enkel guide til å laste ned en nettside med Gleam](https://tylerbrewer.io/blog/how-to-download-a-webpage-with-gleam)