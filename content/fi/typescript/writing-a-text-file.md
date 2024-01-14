---
title:    "TypeScript: Tekstitiedoston kirjoittaminen"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittamalla tekstitiedoston.

Tekstitiedoston kirjoittaminen on tärkeä osa ohjelmointia, sillä se mahdollistaa tiedon tallentamisen ja jakamisen ohjelman suorituksen välillä. Tekstitiedostoja voidaan käyttää monella eri tavalla, esimerkiksi tallentamaan asetuksia tai luomaan tietokantoja.

## Miten

Ohjelmointikieli TypeScript tarjoaa helpon ja tehokkaan tavan kirjoittaa tekstitiedostoja. Seuraavassa on muutamia esimerkkejä, miten voit kirjoittaa tekstitiedoston TypeScriptillä:

```TypeScript
import * as fs from 'fs';

const tekstitiedosto = 'tiedosto.txt';
const data = 'Tämä on esimerkki tekstistä.';

fs.writeFileSync(tekstitiedosto, data);
```

Tämä koodi luo tiedoston nimeltä "tiedosto.txt" ja tallentaa siihen annetun tekstin. Voit myös käyttää muita fs-moduulin metodeita, kuten `appendFileSync` lisätäksesi tekstiä jo olemassa olevaan tiedostoon.

```TypeScript
import * as fs from 'fs';

const tekstitiedosto = 'tiedosto.txt';
const data = 'Lisättyä tekstiä.';

fs.appendFileSync(tekstitiedosto, data);
```

Voit myös lukea tekstitiedoston sisällön käyttämällä `readFileSync`-metodia.

```TypeScript
import * as fs from 'fs';

const tekstitiedosto = 'tiedosto.txt';

const data = fs.readFileSync(tekstitiedosto, 'utf8');
console.log(data); // tulostaa: "Tämä on esimerkki tekstistä. Lisättyä tekstiä."
```

## Syvällisempi tarkastelu

Tekstitiedoston kirjoittaminen TypeScriptillä ei vaadi paljoa koodia, mutta se on silti tärkeä taito ohjelmoinnissa. On myös hyvä huomata, että `fs`-moduulin käyttö voi vaihdella eri alustoilla, joten on tärkeää tutustua sen dokumentaatioon ennen käyttöä.

Voit myös käyttää muita kirjastoja, kuten `fs-extra`, jotka tarjoavat käyttäjäystävällisempiä metodeja tekstitiedostojen käsittelyyn.

## Katso myös

- [Node.js: fs-moduulin dokumentaatio](https://nodejs.org/api/fs.html)
- [fs-extra: käyttäjäystävällisempi vaihtoehto fs-moduulille](https://github.com/jprichardson/node-fs-extra)