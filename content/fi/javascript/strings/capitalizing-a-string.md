---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:46.247668-07:00
description: "Kuinka: JavaScriptiss\xE4 ei ole valmista metodia merkkijonojen alkukirjainten\
  \ suurentamiseen suoraan, mutta sen toteuttaminen perus merkkijonojen\u2026"
lastmod: '2024-03-13T22:44:56.933094-06:00'
model: gpt-4-0125-preview
summary: "JavaScriptiss\xE4 ei ole valmista metodia merkkijonojen alkukirjainten suurentamiseen\
  \ suoraan, mutta sen toteuttaminen perus merkkijonojen k\xE4sittelymetodien avulla\
  \ on suoraviivaista."
title: Merkkijonon muuttaminen isoiksi kirjaimiksi
weight: 2
---

## Kuinka:
JavaScriptissä ei ole valmista metodia merkkijonojen alkukirjainten suurentamiseen suoraan, mutta sen toteuttaminen perus merkkijonojen käsittelymetodien avulla on suoraviivaista.

### Käyttäen standardi JavaScriptiä
```javascript
function capitalize(str) {
  if (!str) return '';
  return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalize('hello world')); // Tuloste: "Hello world"
```

### ES6 Versio
ES6:n mallipohjaisilla literaaleilla funktio voidaan kirjoittaa tiiviimmässä muodossa:
```javascript
const capitalize = (str) => !str ? '' : `${str[0].toUpperCase()}${str.slice(1)}`;

console.log(capitalize('hello ES6')); // Tuloste: "Hello ES6"
```

### Käyttäen Lodashia
Lodash on suosittu kolmannen osapuolen apukirjasto, joka tarjoaa laajan valikoiman funktioita JavaScript-arvojen, mukaan lukien merkkijonojen, käsittelyyn ja työstämiseen. Merkkijonon alkukirjaimen suurentamiseen Lodashin avulla:
```javascript
// Ensin, asenna lodash jos et ole vielä asentanut: npm install lodash
const _ = require('lodash');

console.log(_.capitalize('LODASH example')); // Tuloste: "Lodash example"
```
_Huomaa, kuinka Lodash paitsi suurentaa ensimmäisen kirjaimen myös muuttaa loput merkkijonosta pieniksi kirjaimiksi, mikä eroaa hieman pelkän JavaScriptin toteutuksesta._

### Käyttäen CSS:ää (Vain näyttötarkoituksiin)
Jos tavoitteena on merkkijonon alkukirjaimen suurentaminen käyttöliittymässä näyttämistä varten, voidaan käyttää CSS:ää:
```css
.capitalize {
  text-transform: capitalize;
}
```
```html
<div class="capitalize">hello css</div> <!-- Näkyy muodossa "Hello css" -->
```
**Huomio:** Tämä menetelmä muuttaa tekstin ulkoasua verkkosivulla muuttamatta itse merkkijonoa JavaScriptissä.
