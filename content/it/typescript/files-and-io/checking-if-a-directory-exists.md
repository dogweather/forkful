---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:46.882207-07:00
description: "Come fare: TypeScript, quando eseguito in un ambiente Node.js, consente\
  \ di verificare se una directory esiste utilizzando il modulo `fs`, che fornisce\
  \ la\u2026"
lastmod: '2024-03-13T22:44:43.190440-06:00'
model: gpt-4-0125-preview
summary: TypeScript, quando eseguito in un ambiente Node.js, consente di verificare
  se una directory esiste utilizzando il modulo `fs`, che fornisce la funzione `existsSync()`
  o la funzione asincrona `access()` combinata con `constants.F_OK`.
title: Verifica se una directory esiste
weight: 20
---

## Come fare:
TypeScript, quando eseguito in un ambiente Node.js, consente di verificare se una directory esiste utilizzando il modulo `fs`, che fornisce la funzione `existsSync()` o la funzione asincrona `access()` combinata con `constants.F_OK`.

### Usando `fs.existsSync()`:
```typescript
import { existsSync } from 'fs';

const directoryPath = './path/to/directory';

if (existsSync(directoryPath)) {
  console.log('La directory esiste.');
} else {
  console.log('La directory non esiste.');
}
```

### Usando `fs.access()` con `fs.constants.F_OK`:
```typescript
import { access, constants } from 'fs';

const directoryPath = './path/to/directory';

access(directoryPath, constants.F_OK, (err) => {
  if (err) {
    console.log('La directory non esiste.');
    return;
  }
  console.log('La directory esiste.');
});
```

**Output campione** per entrambi i metodi, assumendo che la directory esista:
```
La directory esiste.
```

E se non esiste:
```
La directory non esiste.
```

### Usando una Libreria di Terze Parti - `fs-extra`:
`fs-extra` è una popolare libreria di terze parti che potenzia il modulo `fs` incorporato e fornisce funzioni più convenienti.

```typescript
import { pathExists } from 'fs-extra';

const directoryPath = './path/to/directory';

pathExists(directoryPath).then(exists => {
  console.log(`La directory esiste: ${exists}`);
});
```

**Output campione** quando la directory esiste:
```
La directory esiste: true
```

E se non esiste:
```
La directory esiste: false
```
