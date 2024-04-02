---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:19.325133-07:00
description: "JSON (JavaScript Object Notation) on kevyt datanvaihtoformaatti, joka\
  \ on ihmisten luettavissa ja kirjoitettavissa sek\xE4 koneiden j\xE4sent\xE4m\xE4\
  ss\xE4 ja\u2026"
lastmod: '2024-03-13T22:44:56.971184-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) on kevyt datanvaihtoformaatti, joka on\
  \ ihmisten luettavissa ja kirjoitettavissa sek\xE4 koneiden j\xE4sent\xE4m\xE4ss\xE4\
  \ ja\u2026"
title: "Ty\xF6skentely JSON:n kanssa"
weight: 38
---

## Mikä & Miksi?

JSON (JavaScript Object Notation) on kevyt datanvaihtoformaatti, joka on ihmisten luettavissa ja kirjoitettavissa sekä koneiden jäsentämässä ja generoimassa helposti. Ohjelmoijat käyttävät sitä datan tallentamiseen ja kuljettamiseen web-sovelluksissa, tehden siitä nykyaikaisten APIen ja web-palveluiden kommunikaation selkärangan.

## Kuinka:

### JSONin jäsentäminen
JSON-merkkijonon muuntamiseksi JavaScript-objektiksi, käytä `JSON.parse()`.

```javascript
const jsonString = '{"name":"John", "age":30, "city":"New York"}';
const obj = JSON.parse(jsonString);
console.log(obj.name); // Tuloste: John
```

### JavaScript-objektien merkkijonoksi muuntaminen
JavaScript-objektin muuntamiseksi takaisin JSON-merkkijonoksi, käytä `JSON.stringify()`.

```javascript
const user = { name: "Jane", age: 25, city: "London" };
const jsonString = JSON.stringify(user);
console.log(jsonString); // Tuloste: {"name":"Jane","age":25,"city":"London"}
```

### Tiedostojen käsittely Node.js:ssä
JSON-tiedoston lukemiseksi ja sen muuntamiseksi objektiksi Node.js-ympäristössä, voit käyttää `fs`-moduulia. Tässä esimerkissä oletetaan, että sinulla on tiedosto nimeltä `data.json`.

```javascript
const fs = require('fs');

fs.readFile('data.json', 'utf-8', (err, data) => {
    if (err) heitä err;
    const obj = JSON.parse(data);
    console.log(obj);
});
```

Objektin kirjoittamiseksi JSON-tiedostoon:

```javascript
const fs = require('fs');
const user = { name: "Mike", age: 22, city: "Berlin" };

fs.writeFile('user.json', JSON.stringify(user, null, 2), (err) => {
    if (err) heitä err;
    console.log('Data kirjoitettu tiedostoon');
});
```

### Kolmannen osapuolen kirjastot
Monimutkaisiin JSON-operaatioihin, kehykset ja kirjastot kuten `lodash` voivat yksinkertaistaa tehtäviä, mutta perusoperaatioihin, natiivit JavaScript-funktiot ovat usein riittäviä. Suurille tai suorituskyvyltään kriittisille sovelluksille, voit harkita kirjastoja kuten `fast-json-stringify` nopeampaan JSON-merkkijonoksi muuntamiseen tai `json5` joustavammassa JSON-formaatissa jäsentämiseen ja merkkijonoksi muuntamiseen.

Jäsentäminen `json5` avulla:
```javascript
const JSON5 = require('json5');

const jsonString = '{name:"John", age:30, city:"New York"}';
const obj = JSON5.parse(jsonString);
console.log(obj.name); // Tuloste: John
```

Nämä esimerkit kattavat perusoperaatiot JSONin kanssa JavaScriptissä, täydellisiä aloittelijoille, jotka siirtyvät toisista kielistä ja haluavat käsitellä tietoja web-sovelluksissa tehokkaasti.
