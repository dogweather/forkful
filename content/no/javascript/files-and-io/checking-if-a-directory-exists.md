---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:45.985179-07:00
description: "Hvordan: I Node.js, siden JavaScript i seg selv ikke har direkte tilgang\
  \ til filsystemet, brukes `fs`-modulen typisk for slike operasjoner. Her er en\u2026"
lastmod: '2024-03-13T22:44:41.197644-06:00'
model: gpt-4-0125-preview
summary: I Node.js, siden JavaScript i seg selv ikke har direkte tilgang til filsystemet,
  brukes `fs`-modulen typisk for slike operasjoner.
title: Sjekker om en mappe eksisterer
weight: 20
---

## Hvordan:
I Node.js, siden JavaScript i seg selv ikke har direkte tilgang til filsystemet, brukes `fs`-modulen typisk for slike operasjoner. Her er en enkel måte å sjekke om en mappe finnes ved bruk av `fs.existsSync()`:

```javascript
const fs = require('fs');

const directoryPath = './sample-directory';

// Sjekk om mappen finnes
if (fs.existsSync(directoryPath)) {
  console.log('Mappen finnes.');
} else {
  console.log('Mappen finnes ikke.');
}
```
**Eksempel på utskrift:**
```
Mappen finnes.
```
Eller, for en ikke-blokkerende asynkron tilnærming, bruk `fs.promises` med `async/await`:

```javascript
const fs = require('fs').promises;

async function checkDirectory(directoryPath) {
  try {
    await fs.access(directoryPath);
    console.log('Mappen finnes.');
  } catch (error) {
    console.log('Mappen finnes ikke.');
  }
}

checkDirectory('./sample-directory');
```
**Eksempel på utskrift:**
```
Mappen finnes.
```

For prosjekter som i stor grad benytter seg av fil- og mappeoperasjoner, tilbyr `fs-extra`-pakken, en utvidelse av den native `fs`-modulen, praktiske tilleggsfunksjoner. Her er hvordan du kan oppnå det samme med `fs-extra`:

```javascript
const fs = require('fs-extra');

const directoryPath = './sample-directory';

// Sjekk om mappen finnes
fs.pathExists(directoryPath)
  .then(exists => console.log(exists ? 'Mappen finnes.' : 'Mappen finnes ikke.'))
  .catch(err => console.error(err));
```
**Eksempel på utskrift:**
```
Mappen finnes.
```

Denne tilnærmingen muliggjør ren, lesbar kode som smidig integreres med moderne JavaScript-praksiser.
