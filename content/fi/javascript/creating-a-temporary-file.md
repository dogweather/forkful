---
title:                "Väliaikaisen tiedoston luominen"
html_title:           "Javascript: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Väliaikaisten tiedostojen luominen on tärkeä osa ohjelmointia, joka mahdollistaa ohjelmien tehokkaan toiminnan. Väliaikaiset tiedostot ovat tilapäisiä tiedostoja, jotka luodaan tiettyyn tarkoitukseen ja poistetaan sen jälkeen. Ohjelmoijat käyttävät väliaikaisia tiedostoja useisiin tarkoituksiin, kuten välimuistin hallintaan, väliaikaisten tietojen tallentamiseen ja ohjelman suorituksen optimointiin.

## Kuinka se tehdään?

Väliaikaisen tiedoston luominen Javascriptillä on helppoa. Käytä vain `fs` (file system) -moduulia ja sen `mktempSync`-toimintoa. Alla on yksinkertainen koodiesimerkki, joka luo väliaikaisen tiedoston ja tulostaa sen nimen konsoliin:

```Javascript
const fs = require('fs');
const tmpFile = fs.mktempSync('tmpfile-XXXXXX');
console.log(tmpFile); 
```

Kun suoritat tämän koodin, saat tulosteena luodun väliaikaisen tiedoston nimen, kuten `tmpfile-nAsfzP`. Tämän jälkeen voit käyttää tätä tiedostoa ohjelmassasi tarpeen mukaan ja poistaa sen lopuksi.

## Syvempää tietoa

Väliaikaisten tiedostojen luominen on ollut osa ohjelmointia jo pitkään. Aiemmin ohjelmoijat joutuivat käsittelemään tiedostojen nimiä ja luomaan ne itse. Nykyään `fs` -moduulin avulla tiedoston luominen on yksinkertaistunut ja nopeutunut.

On myös olemassa muita tapoja luoda väliaikaisia tiedostoja, kuten käyttäen muita ohjelmointikieliä tai tietokantoja. Kuitenkin Javascript on yleisimmin käytetty ja moderni tapa luoda väliaikaisia tiedostoja.

## Katso myös

- [fs - Node.js docs](https://nodejs.org/docs/latest-v12.x/api/fs.html)
- [Creating a Temporary File in JavaScript](https://www.javascripttutorial.net/javascript-file/)