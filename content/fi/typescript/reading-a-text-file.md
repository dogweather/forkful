---
title:    "TypeScript: Tiedoston lukeminen"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tiedostojen käsittely muodostaa tärkeän osan ohjelmoinnista. Tekstitiedostojen lukeminen on yksi yleisimmistä toiminnoista koodissa ja se voi olla hyödyllistä esimerkiksi tietokantojen päivittämisessä tai toisesta ohjelmasta saatujen tietojen käsittelyssä.

## Kuinka

```TypeScript
import * as fs from 'fs';

// Funktio, joka lukee tekstirivin tiedostosta ja palauttaa sen merkkijonona
function lueTiedosto(rivi: number, tiedosto: string): string {
  // Avataan tiedosto
  let data = fs.readFileSync(tiedosto, 'utf8');
  // Jaetaan tiedosto rivin lopussa olevan rivinvaihdon kohdalta
  let rivit = data.split('\n');
  // Palautetaan haluttu rivi - 1, sillä taulukon indeksit alkavat nollasta
  return rivit[rivi - 1];
}

// Tiedostossa "data.txt" on teksti "Tämä on esimerkki"
// Tulostaa "Tämä on esimerkki"
console.log(lueTiedosto(1, 'data.txt'));
```

## Syvällisempi tarkastelu

Koodiesimerkissä käytetään Node.js:n "fs" -moduulia, joka antaa meille mahdollisuuden käsitellä tiedostoja TypeScript:ssä. Funktio "lueTiedosto" hyödyntää "fs.readFileSync" -metodia, joka lukee tiedoston halutusta polusta ja merkistökoodauksesta. "split" -metodilla jaetaan tiedosto rivien lopussa olevan rivinvaihdon kohdalta ja palautetaan halutun rivin sisältö.

## Katso myös

- [Node.js fs-moduuli](https://nodejs.org/api/fs.html)
- [TypeScriptin tietotyypit](https://www.typescriptlang.org/docs/handbook/basic-types.html)
- [Tiedostojen käsittelyn perusteet TypeScriptissä](https://www.educba.com/file-handling-in-typescript/)