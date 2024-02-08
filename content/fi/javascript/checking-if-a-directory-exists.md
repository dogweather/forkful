---
title:                "Tarkistetaan, onko hakemisto olemassa"
aliases:
- fi/javascript/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:50.658188-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?
Tiedoston olemassaolon tarkistaminen JavaScriptillä on olennainen osa tiedostojen käsittelytehtäviä, mikä mahdollistaa skriptien varmistaa kansion olemassaolon ennen siitä lukemista tai siihen kirjoittamista. Tämä toiminto estää virheitä ja varmistaa ohjelman sujuvamman suorituksen, erityisesti sovelluksissa, jotka käsittelevät tiedostoja tai kansioita dynaamisesti käyttäjän syötteen tai ulkoisten tietolähteiden perusteella.

## Kuinka:
Node.js:ssä, koska JavaScriptillä itsellään ei ole suoraa pääsyä tiedostojärjestelmään, yleensä käytetään `fs`-moduulia tällaisiin operaatioihin. Tässä on yksinkertainen tapa tarkistaa, onko kansio olemassa käyttäen `fs.existsSync()`:

```javascript
const fs = require('fs');

const directoryPath = './sample-directory';

// Tarkista, onko kansio olemassa
if (fs.existsSync(directoryPath)) {
  console.log('Kansio on olemassa.');
} else {
  console.log('Kansiota ei ole olemassa.');
}
```
**Esimerkkituloste:**
```
Kansio on olemassa.
```
Tai ei-estävää asynkronista lähestymistapaa varten, käytä `fs.promises` yhdessä `async/await` kanssa:

```javascript
const fs = require('fs').promises;

async function checkDirectory(directoryPath) {
  try {
    await fs.access(directoryPath);
    console.log('Kansio on olemassa.');
  } catch (error) {
    console.log('Kansiota ei ole olemassa.');
  }
}

checkDirectory('./sample-directory');
```
**Esimerkkituloste:**
```
Kansio on olemassa.
```

Projekteissa, jotka käyttävät runsaasti tiedosto- ja kansio-operaatioita, `fs-extra`-paketti, joka on natiivin `fs`-moduulin laajennus, tarjoaa käteviä lisämetodeja. Tässä on, miten voit saavuttaa saman `fs-extra` käyttäen:

```javascript
const fs = require('fs-extra');

const directoryPath = './sample-directory';

// Tarkista, onko kansio olemassa
fs.pathExists(directoryPath)
  .then(exists => console.log(exists ? 'Kansio on olemassa.' : 'Kansiota ei ole olemassa.'))
  .catch(err => console.error(err));
```
**Esimerkkituloste:**
```
Kansio on olemassa.
```

Tämä lähestymistapa mahdollistaa puhtaan, luettavan koodin, joka integroituu saumattomasti modernien JavaScript-käytäntöjen kanssa.
