---
title:                "Tarkistaako hakemisto on olemassa"
html_title:           "TypeScript: Tarkistaako hakemisto on olemassa"
simple_title:         "Tarkistaako hakemisto on olemassa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Kun ohjelmointia tehdään, saattaa joskus olla tarpeen tarkistaa, onko hakemistoa olemassa. Hakemisto on tietokoneen osa, johon tallennetaan tiedostoja. Ohjelmoijat voivat tarkistaa hakemiston olemassaolon, jotta he voivat tehdä päätöksiä sen suhteen, miten käsitellä mahdollisia tiedostoja kyseisessä hakemistossa.

## How to:

Tarkistaaksesi, onko hakemisto olemassa, voit käyttää seuraavaa TypeScript-koodia:

```TypeScript
import * as fs from 'fs';
 
// Tarkistetaan, onko hakemisto olemassa
fs.existsSync('/polku/hakemistoon');
```

Tämä koodi käyttää Node.js-tiedostojärjestelmäkirjastoa (fs) ja sen metodia existsSync () tarkistaakseen, onko annettu hakemistopolku olemassa. Metodi palauttaa totuusarvon (true tai false) riippuen siitä, oliko hakemisto olemassa vai ei. Voit sitten käyttää tätä tietoa päätöksenteossa ohjelmassasi.

## Deep Dive:

Historiallisesti ajatellen, hakemistojen olemassaolon tarkistaminen on ollut tärkeää varsinkin silloin, kun työskennellään monenlaisten tietokonejärjestelmien kanssa, joissa on erilaiset tiedostojärjestelmät. Nykyään tämä taito on edelleen hyödyllinen ja yleisesti käytetty ohjelmoinnissa.

On myös muita tapoja tarkistaa, onko hakemisto olemassa, kuten käyttämällä asetuspyynnön metodeja (request methods) tai käyttämällä ohjelmistokirjastoa, kuten "fs-extra". Nämä menetelmät voivat olla hyödyllisiä, jos tarvitset lisäominaisuuksia tarkistuksellesi.

Node.js-tiedostojärjestelmäkirjasto tarjoaa myös muita metodeja tiedostojen ja hakemistojen käsittelyyn. Voit tutustua niihin tarkemmin Node.js-dokumentaatiossa.

## See Also:

- [Node.js - Tiedostojärjestelmäkirjasto (fs)](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html)
- [Node.js - Asetuspyynnöt (request methods)](https://nodejs.org/dist/latest-v14.x/docs/api/http.html#http_http_request_url_options_callback)
- [fs-extra - Ohjelmistokirjasto](https://github.com/jprichardson/node-fs-extra)