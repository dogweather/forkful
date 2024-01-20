---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "Arduino: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Tekstitiedostojen kirjoittamista käytetään tiedon tallentamiseen pysyvästi. Koodaajille se tarjoaa yksinkertaisen tavan säilöä sovelluksen dataa tai lokitietoja.

## How to:
Asenna ensin tiedostojen käsittelyyn tarvittava Node.js `fs`-moduuli:

```TypeScript
import * as fs from 'fs';

fs.writeFile('tervehdys.txt', 'Hei vaan TypeScriptin ystävät!', err => {
  if (err) throw err;
  console.log('Tiedosto on tallennettu!');
});
```

Koodi luo tekstitiedoston nimeltä "tervehdys.txt" ja kirjoittaa siihen viestin. Jos onnistuu, konsoliin tulostuu ilmoitus.

## Deep Dive
Typescript on kehittynyt JavaScriptin supersetiksi, joka mahdollistaa tyypitetyn ohjelmoinnin. Se on saanut vaikutteita aiemmista ohjelmointikielistä, kuten Java ja C#. Aikaisemmin tiedostojen käsittelyyn käytettiin pelkkää JavaScriptia, mutta TypeScript tarjoaa vahvan tyypityksen ja luokkien tuoman selkeyden. Vaihtoehtoina tiedostojen kirjoittamiseen voivat olla erilaiset tietokannat tai pilvipalvelut, mutta perinteinen tiedostojärjestelmän käyttö on edelleen arvokasta nopean kehityksen ja helpon testauksen vuoksi.

## See Also
- [Node.js File System -dokumentaatio](https://nodejs.org/api/fs.html)
- [TypeScriptin viralliset ohjeet](https://www.typescriptlang.org/docs/)
- [Modernin JavaScriptin oppiminen](https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript)