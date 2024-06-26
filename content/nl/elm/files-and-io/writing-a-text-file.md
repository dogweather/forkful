---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:59.505873-07:00
description: "Hoe: Elm is een front-end webtaal, dus het kan niet rechtstreeks bestanden\
  \ naar een schijf schrijven. Maar het kan wel een download activeren met de\u2026"
lastmod: '2024-03-13T22:44:50.743632-06:00'
model: gpt-4-0125-preview
summary: Elm is een front-end webtaal, dus het kan niet rechtstreeks bestanden naar
  een schijf schrijven.
title: Een tekstbestand schrijven
weight: 24
---

## Hoe:
Elm is een front-end webtaal, dus het kan niet rechtstreeks bestanden naar een schijf schrijven. Maar het kan wel een download activeren met de gewenste inhoud. Om het schrijven van een bestand te simuleren, zullen we een tekst creëren en een link gebruiken om het als een bestand te downloaden.

```Elm
module Main exposing (main)

import Browser
import Html exposing (Html, a, text, attribute)
import Html.Attributes exposing (href)

createTextFileContent : String
createTextFileContent =
    "Hallo, Wereld! Dit is wat inhoud."

createDownloadHref : String -> String
createDownloadHref inhoud =
    "data:text/plain;charset=utf-8," ++ encodeURIComponent(inhoud)

main : Html msg
main =
    a [ href (createDownloadHref createTextFileContent), attribute "download" "mijnTekstBestand.txt" ]
        [ text "Tekstbestand Downloaden" ]
```

Het voorbeeldresultaat is een klikbare link die 'mijnTekstBestand.txt' downloadt met de inhoud "Hallo, Wereld! Dit is wat inhoud."

## Diepere Duik
Elm draait in de browser, dus functies die nodig zijn om rechtstreeks naar het bestandssysteem te schrijven zijn niet beschikbaar. Historisch gezien heeft JavaScript vergelijkbare beperkingen vanwege beveiligingsbeperkingen van de browser. Echter, nieuwere web-API's en Elm's interop-functie (`Ports`) maken het mogelijk om downloads te activeren of toegang tot het bestandssysteem in webapplicaties te behandelen. Alternatieven zijn het gebruik van server-side programmeertalen voor directe bestandsmanipulatie of het vertrouwen op web-API's zoals de File System Access API voor uitgebreide mogelijkheden in moderne browsers.

## Zie Ook
- Elm Officiële Gids over JavaScript Interop (Ports): [Elm Ports](https://guide.elm-lang.org/interop/ports.html)
- De `File` Web API voor geavanceerde bestandsafhandeling in browsers: [MDN Web Docs - Bestands-API](https://developer.mozilla.org/en-US/docs/Web/API/File)
- Een bredere blik op de Elm-architectuur: [Officiële Elm Architectuur](https://guide.elm-lang.org/architecture/)
