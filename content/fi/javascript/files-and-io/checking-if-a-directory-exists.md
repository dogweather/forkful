---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:50.658188-07:00
description: "Kuinka: Node.js:ss\xE4, koska JavaScriptill\xE4 itsell\xE4\xE4n ei ole\
  \ suoraa p\xE4\xE4sy\xE4 tiedostoj\xE4rjestelm\xE4\xE4n, yleens\xE4 k\xE4ytet\xE4\
  \xE4n `fs`-moduulia t\xE4llaisiin operaatioihin.\u2026"
lastmod: '2024-03-13T22:44:56.964257-06:00'
model: gpt-4-0125-preview
summary: "Node.js:ss\xE4, koska JavaScriptill\xE4 itsell\xE4\xE4n ei ole suoraa p\xE4\
  \xE4sy\xE4 tiedostoj\xE4rjestelm\xE4\xE4n, yleens\xE4 k\xE4ytet\xE4\xE4n `fs`-moduulia\
  \ t\xE4llaisiin operaatioihin."
title: Tarkistetaan, onko hakemisto olemassa
weight: 20
---

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
