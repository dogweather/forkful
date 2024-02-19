---
aliases:
- /fi/typescript/writing-a-text-file/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:34.985667-07:00
description: "Tekstitiedoston kirjoittaminen TypeScriptill\xE4 on kriittinen taito\
  \ datan pysyvyyden, konfiguraatioiden tai lokiin kirjoittamisen kannalta. Ohjelmoijat\u2026"
lastmod: 2024-02-18 23:09:07.338583
model: gpt-4-0125-preview
summary: "Tekstitiedoston kirjoittaminen TypeScriptill\xE4 on kriittinen taito datan\
  \ pysyvyyden, konfiguraatioiden tai lokiin kirjoittamisen kannalta. Ohjelmoijat\u2026"
title: Tekstitiedoston kirjoittaminen
---

{{< edit_this_page >}}

## Mikä ja miksi?
Tekstitiedoston kirjoittaminen TypeScriptillä on kriittinen taito datan pysyvyyden, konfiguraatioiden tai lokiin kirjoittamisen kannalta. Ohjelmoijat suorittavat usein tämän tehtävän tallentaakseen ja manipuloidakseen tietoa sovelluksen muistin ulkopuolella syistä kuten datan analysointi, raportointi tai yksinkertaisesti käyttäjäasetusten tallentaminen istuntojen välillä.

## Kuinka:
TypeScript ei itsessään käsittele suoraan tiedosto-operaatioita, sillä se kääntyy JavaScriptiksi, joka perinteisesti toimii selaimessa rajoitetulla pääsyllä tiedostojärjestelmään. Kuitenkin, kun sitä käytetään Node.js-ympäristössä, `fs`-moduuli (File System) tarjoaa toiminnallisuuden tiedostojen kirjoittamiseen.

### Node.js fs-moduulin käyttäminen
Varmista ensin, että työskentelet Node.js-ympäristössä. Käytä sitten `fs`-moduulia teksti-tiedostojen kirjoittamiseen. Tässä on yksinkertainen esimerkki:

```typescript
import * as fs from 'fs';

const data = 'Hello, world!';
const filePath = './message.txt';

fs.writeFile(filePath, data, 'utf8', (err) => {
    if (err) throw err;
    console.log('Tiedosto on tallennettu!');
});
```

Tämä kirjoittaa asynkronisesti "Hello, world!" `message.txt`-tiedostoon. Jos tiedostoa ei ole olemassa, Node.js luo sen; jos on, Node.js ylikirjoittaa sen.

Synkroniseen tiedoston kirjoittamiseen, käytä `writeFileSync`:

```typescript
import * as fs from 'fs';

const data = 'Hello again, world!';
const filePath = './message.txt';

try {
    fs.writeFileSync(filePath, data, 'utf8');
    console.log('Tiedosto on tallennettu!');
} catch (err) {
    console.error(err);
}
```

### Suosittujen kolmannen osapuolen kirjastojen käyttäminen
Vaikka alkuperäinen `fs`-moduuli on tehokas, jotkin kehittäjät suosivat kolmannen osapuolen kirjastojen käyttöä lisämukavuuden ja toiminnallisuuden vuoksi. `fs-extra` on suosittu valinta, joka laajentaa `fs`:ää ja tekee tiedosto-operaatioista suoraviivaisempia.

Asenna ensin `fs-extra`:

```
npm install fs-extra
```

Sitten voit käyttää sitä TypeScript-tiedostossasi kirjoittaaksesi tekstisisältöä:

```typescript
import * as fs from 'fs-extra';

const data = 'Tämä on fs-extra!';
const filePath = './extraMessage.txt';

// Käyttäen async/await
async function writeFile() {
    try {
        await fs.writeFile(filePath, data, 'utf8');
        console.log('Tiedosto on tallennettu fs-extra:n avulla!');
    } catch (err) {
        console.error(err);
    }
}

writeFile();
```

Tämä koodinpätkä tekee saman asian kuin aiemmat `fs`-esimerkit, mutta hyödyntää `fs-extra`-kirjastoa, tarjoten puhtaamman syntaksin lupausten käsittelyyn.
