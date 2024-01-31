---
title:                "JSON-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Arduino: JSON-tiedostojen käsittely"
simple_title:         "JSON-tiedostojen käsittely"

category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON eli JavaScript Object Notation on kevyt datanvaihtoformaatti. Ohjelmoijat käyttävät sitä tiedon tallentamiseen ja verkossa siirtämiseen, koska se on helppolukuinen ja -kirjoitettava sekä koneille että ihmisille.

## How to:
Yksinkertainen TypeScript-esimerkki JSONin käsittelystä:

```TypeScript
// Määritellään TypeScript-tyyppi
type Henkilo = {
  nimi: string;
  ika: number;
  harrastukset?: string[]; // vapaaehtoinen kenttä
};

// Esimerkki JSON-olio
const json = `{
  "nimi": "Maija",
  "ika": 30,
  "harrastukset": ["juoksu", "lukeminen"]
}`;

// Miten muunnetaan JSON-merkkijono TypeScript-olioksi
const henkilo: Henkilo = JSON.parse(json);

console.log(henkilo.nimi); // Output: Maija

// Miten muunnetaan TypeScript-olio JSON-merkkijonoksi
const uusiJson = JSON.stringify(henkilo);
console.log(uusiJson); // Output: {"nimi":"Maija","ika":30,"harrastukset":["juoksu","lukeminen"]}
```

## Deep Dive
JSON kehitettiin alun perin JavaScript-kielen osana, mutta nykyään se on itsenäinen standardi, jota käytetään kieliriippumattomana datanvaihtoformaatina. Alternatiiveiksi JSONille voidaan mainita XML ja YAML, mutta JSON on ylivoimaisen suosittu osittain sen yksinkertaisuuden ja loogisen rakenteen takia. JSONin käsittely TypeScriptissä on suoraviivaista: `JSON.parse()` muuntaa JSON-merkkijonon TypeScript-olioksi, ja `JSON.stringify()` muuttaa TypeScript-olion takaisin JSON-merkkijonoksi.

## See Also
- MDN Web Docs, JSON: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON)
- TypeScriptin viralliset sivut: [https://www.typescriptlang.org/](https://www.typescriptlang.org/)
- JSON-spesifikaatio: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
