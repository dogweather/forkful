---
title:                "Verifica dell'esistenza di una directory"
date:                  2024-01-20T14:58:50.954414-07:00
html_title:           "Gleam: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Controllare l'esistenza di una directory significa verificare se una cartella è presente nel file system. I programmatori lo fanno per evitare errori durante l'accesso ai file, la lettura o la scrittura, garantendo che l'operazione sia sicura.

## How to:
Usare `fs` di Node.js per verificare se una directory esiste in TypeScript, installate prima `@types/node` per i tipi:

```bash
npm install --save-dev @types/node
```

Ecco un esempio di codice:

```typescript
import * as fs from 'fs';
import { promisify } from 'util';

// Convert fs.exists into a promise-based function
const exists = promisify(fs.exists);

async function checkDirectory(directoryPath: string): Promise<void> {
  const directoryExists = await exists(directoryPath);

  console.log(directoryExists 
    ? `La directory esiste: ${directoryPath}` 
    : `La directory non esiste: ${directoryPath}`);
}

// Usa la funzione e stampa il risultato
checkDirectory('./esempio-directory').then(() => process.exit());
```

Output possibile:

```
La directory esiste: ./esempio-directory
```

Oppure:

```
La directory non esiste: ./esempio-directory
```

## Deep Dive:
`fs.exists` veniva usato in passato, ma ora è deprecato perché non fornisce errori specifici. Invece si consiglia `fs.access` o `fs.stat`. `fs.access` verifica i permessi, mentre with `fs.stat` si ottengono informazioni dettagliate dell'entità file. Ecco le alternative moderne:

```typescript
import { promises as fsPromises } from 'fs';

async function checkDirectoryNew(directoryPath: string): Promise<void> {
  try {
    await fsPromises.access(directoryPath);
    console.log(`La directory esiste: ${directoryPath}`);
  } catch (error) {
    console.error(`La directory non esiste: ${directoryPath}`);
  }
}

// Usage
checkDirectoryNew('./nuova-esempio-directory').then(() => process.exit());
```

`fsPromises.stat` è utile se volete anche altre informazioni, come la dimensione della directory.

## See Also:
- Node.js 'fs' module: https://nodejs.org/api/fs.html
- `fsPromises.access`: https://nodejs.org/api/fs.html#fspromisesaccesspath-mode
- `fsPromises.stat`: https://nodejs.org/api/fs.html#fspromisesstatpath-options
- NPM '@types/node': https://www.npmjs.com/package/@types/node
