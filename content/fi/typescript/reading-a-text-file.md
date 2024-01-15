---
title:                "Tiedostojen lukeminen"
html_title:           "TypeScript: Tiedostojen lukeminen"
simple_title:         "Tiedostojen lukeminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit lukea tiedostoa koodin avulla? Tiedostojen lukeminen on yleinen tehtävä, kun käsitellään dataa tai tallennetaan tietoja pysyvästi ohjelmassa. Tässä artikkelissa opit, miten voit lukea tekstiäiedostoja TypeScriptillä ja miten voit hyödyntää niitä koodissasi.

## Miten

### Tiedoston lukeminen

Aloita luomalla uusi TypeScript-tiedosto ja nimettämällä se "textreader.ts". Ensimmäiseksi, tuodaan Node.js:n "fs" moduuli, joka mahdollistaa tiedostoihin pääsyn. Voit tehdä tämän kirjoittamalla:

```
TypeScript
import * as fs from 'fs';
```

Nyt olemme valmiita lukemaan tiedoston. Voimme käyttää "fs" moduulin "readFileSync" funktiota, joka lukee tiedoston synkronisesti ja palauttaa sen sisällön merkkijonona. Se näyttää tältä:

```
TypeScript
const content: string = fs.readFileSync('test.txt', 'utf8');
```

### Tulostaminen konsoliin

Nyt kun olemme lukeneet tiedoston, voimme tulostaa sen sisällön konsoliin käyttämällä "console.log" funktiota. Se näyttää tältä:

```
TypeScript
console.log(content);
```

Kun suoritat tämän koodin, sinun pitäisi nähdä tiedoston sisältö konsolissa. Varmista, että sinulla on teksti tiedostossa nimeltä "test.txt" samassa kansiossa kuin TypeScript-tiedostosi.

## Syvemmälle

Tiedoston lukeminen on monipuolinen prosessi ja "fs" moduuli tarjoaa monia muita hyödyllisiä toimintoja lisäksi "readFileSync". Voit käydä tutustumassa dokumentaatioon saadaksesi lisätietoja ja oppia lukemaan tiedostoja eri tavoin. Lisäksi voit myös käsitellä tiedoston sisältöä manipuloimalla, suodattamalla ja analysoimalla sitä.

## Katso myös

- [Node.js "fs" moduulin dokumentaatio](https://nodejs.org/api/fs.html)
- [TypeScript dokumentaatio](https://www.typescriptlang.org/docs/)