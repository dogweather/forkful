---
title:                "Kirjoittaminen standardivirheeseen"
aliases:
- fi/elm/writing-to-standard-error.md
date:                  2024-02-03T19:33:09.529952-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kirjoittaminen standardivirheeseen"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mitä & Miksi?

Standardivirheeseen (stderr) kirjoittaminen tarkoittaa virheviestien ja diagnostiikkatietojen ohjaamista erilleen pääohjelman tulosteesta, joka menee vakiotulostukseen (stdout). Ohjelmoijat tekevät näin virheenkäsittelyn ja lokitiedon hallinnan helpottamiseksi, erityisesti ympäristöissä, joissa tulosteen erottaminen on kriittistä vianmäärityksen ja seurannan kannalta.

## Kuinka:

Elm on ensisijaisesti suunnattu web-kehitykseen, jossa suoraan stderriin kirjoittamisen käsite ei päde samalla tavalla kuin perinteisissä komentorivi-ympäristöissä. Kuitenkin, Elm-ohjelmille, jotka toimivat Node.js:ssä tai vastaavissa ympäristöissä, interop JavaScriptin kanssa käyttäen portteja on keskeinen lähestymistapa saavuttaa samanlainen toiminnallisuus. Tässä on, miten saat sen pystytettyä:

Elm-koodi (`Main.elm`):
```elm
port module Main exposing (main)

import Browser

port errorOut : String -> Cmd msg

-- Esimerkkifunktio, joka lähettää virheviestin JS:ään
generateError : String -> Cmd msg
generateError message =
    errorOut message

main =
    generateError "Tämä on virheviesti stderriin"
```

JavaScript-Interop (`index.js`):
```javascript
const { Elm } = require('./Main.elm');

var app = Elm.Main.init();

app.ports.errorOut.subscribe((message) => {
  console.error(message);
});
```

Tämä Elm-koodi määrittelee portin `errorOut`, joka mahdollistaa viestien lähettämisen Elmistä JavaScriptiin. Sitten JavaScript-koodissa kuuntelemme viestejä, jotka on lähetetty tämän portin kautta, ja ohjaamme ne stderriin käyttämällä `console.error()`. Tällä tavalla voit tehokkaasti kirjoittaa stderriin ympäristössä, joka tukee sitä, hyödyntämällä Elmin interop-ominaisuuksia JavaScriptin kanssa.

Esimerkkituloste Node.js-terminaalissa (kun `index.js` ajetaan):
```
Tämä on virheviesti stderriin
```
