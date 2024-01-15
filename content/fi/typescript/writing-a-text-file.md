---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "TypeScript: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kirjoittaisit tekstitiedostoa TypeScriptillä? Yksinkertaisesti siksi, että TypeScript on yksi suosituimmista ohjelmointikielistä ja se tarjoaa helpon tavan käsitellä ja kirjoittaa tekstitiedostoja.

## Miten

Kirjoittaminen tekstitiedostoon TypeScriptillä on todella helppoa ja selkeää. Tässä on muutama esimerkki, jotta voit aloittaa:

```typescript
// Luodaan uusi tiedosto nimeltä "testi.txt"
const fs = require('fs');
fs.writeFileSync("testi.txt", "Tervetuloa lukemaan tämä teksti!");

// Luetaan tiedostosta "testi.txt" ja tallennetaan sisältö muuttujaan
const sisältö = fs.readFileSync("testi.txt", "utf8");
console.log(sisältö); // tulostaa "Tervetuloa lukemaan tämä teksti!"

// Lisätään olemassa olevaan tiedostoon uusi rivi
fs.appendFileSync("testi.txt", "\nJa kiitos kun luit tekstini!");
```

Koodin tulostus:

```
Tervetuloa lukemaan tämä teksti!
Ja kiitos kun luit tekstini!
```

## Syvällinen sukellus

Kirjoittaminen ja lukeminen tekstitiedostoon TypeScriptillä tapahtuu käyttämällä Node.js:n sisäänrakennettua fs-moduulia, joka tarjoaa metodit tiedostojen käsittelyyn. `writeFileSync()`-metodi luo uuden tiedoston ja kirjoittaa siihen annetun sisällön, `readFileSync()`-metodi lukee tiedostosta sisällön ja `appendFileSync()`-metodi lisää uuden rivin olemassa olevaan tiedostoon. Huomaa, että metodit käyttävät ensimmäisenä parametrina tiedoston nimeä ja toisena parametrina sisältöä, joka voi olla sekä tekstiä että muuta tietotyyppiä.

## Katso myös

- [Node.js fs-moduuli](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html)
- [TypeScriptin virallinen dokumentaatio](https://www.typescriptlang.org/)