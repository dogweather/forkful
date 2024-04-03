---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:01.138034-07:00
description: "Come fare: In Node.js, poich\xE9 JavaScript di per s\xE9 non ha accesso\
  \ diretto al sistema dei file, si utilizza tipicamente il modulo `fs` per tali operazioni.\u2026"
lastmod: '2024-03-13T22:44:43.827292-06:00'
model: gpt-4-0125-preview
summary: "In Node.js, poich\xE9 JavaScript di per s\xE9 non ha accesso diretto al\
  \ sistema dei file, si utilizza tipicamente il modulo `fs` per tali operazioni."
title: Verifica se una directory esiste
weight: 20
---

## Come fare:
In Node.js, poiché JavaScript di per sé non ha accesso diretto al sistema dei file, si utilizza tipicamente il modulo `fs` per tali operazioni. Ecco un modo semplice per verificare se una directory esiste usando `fs.existsSync()`:

```javascript
const fs = require('fs');

const directoryPath = './sample-directory';

// Verifica se la directory esiste
if (fs.existsSync(directoryPath)) {
  console.log('La directory esiste.');
} else {
  console.log('La directory non esiste.');
}
```
**Output dell'esempio:**
```
La directory esiste.
```
Oppure, per un approccio asincrono non bloccante, usare `fs.promises` con `async/await`:

```javascript
const fs = require('fs').promises;

async function verificaDirectory(directoryPath) {
  try {
    await fs.access(directoryPath);
    console.log('La directory esiste.');
  } catch (error) {
    console.log('La directory non esiste.');
  }
}

verificaDirectory('./sample-directory');
```
**Output dell'esempio:**
```
La directory esiste.
```

Per progetti che fanno largo uso di operazioni su file e directory, il pacchetto `fs-extra`, un'estensione del modulo nativo `fs`, offre metodi aggiuntivi comodi. Ecco come si può fare lo stesso con `fs-extra`:

```javascript
const fs = require('fs-extra');

const directoryPath = './sample-directory';

// Verifica se la directory esiste
fs.pathExists(directoryPath)
  .then(exists => console.log(exists ? 'La directory esiste.' : 'La directory non esiste.'))
  .catch(err => console.error(err));
```
**Output dell'esempio:**
```
La directory esiste.
```

Questo approccio consente di scrivere codice pulito e leggibile che si integra senza problemi con le pratiche moderne di JavaScript.
