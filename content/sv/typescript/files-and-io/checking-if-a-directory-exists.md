---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:45.468259-07:00
description: "Hur: TypeScript, n\xE4r det k\xF6rs i en Node.js-milj\xF6, till\xE5\
  ter dig att kontrollera om en mapp finns genom att anv\xE4nda `fs`-modulen, som\
  \ erbjuder funktionen\u2026"
lastmod: '2024-03-13T22:44:37.670418-06:00'
model: gpt-4-0125-preview
summary: "TypeScript, n\xE4r det k\xF6rs i en Node.js-milj\xF6, till\xE5ter dig att\
  \ kontrollera om en mapp finns genom att anv\xE4nda `fs`-modulen, som erbjuder funktionen\
  \ `existsSync()` eller den asynkrona funktionen `access()` kombinerat med `constants.F_OK`."
title: Kontrollera om en katalog existerar
weight: 20
---

## Hur:
TypeScript, när det körs i en Node.js-miljö, tillåter dig att kontrollera om en mapp finns genom att använda `fs`-modulen, som erbjuder funktionen `existsSync()` eller den asynkrona funktionen `access()` kombinerat med `constants.F_OK`.

### Använda `fs.existsSync()`:
```typescript
import { existsSync } from 'fs';

const directoryPath = './path/to/directory';

if (existsSync(directoryPath)) {
  console.log('Mappen finns.');
} else {
  console.log('Mappen finns inte.');
}
```

### Använda `fs.access()` med `fs.constants.F_OK`:
```typescript
import { access, constants } from 'fs';

const directoryPath = './path/to/directory';

access(directoryPath, constants.F_OK, (err) => {
  if (err) {
    console.log('Mappen finns inte.');
    return;
  }
  console.log('Mappen finns.');
});
```

**Exempelutskrift** för båda metoderna, med antagandet att mappen finns:
```
Mappen finns.
```

Och om den inte gör det:
```
Mappen finns inte.
```

### Använda ett tredjepartsbibliotek - `fs-extra`:
`fs-extra` är ett populärt tredjepartsbibliotek som förbättrar den inbyggda `fs`-modulen och tillhandahåller mer bekväma funktioner.

```typescript
import { pathExists } from 'fs-extra';

const directoryPath = './path/to/directory';

pathExists(directoryPath).then(exists => {
  console.log(`Mappen finns: ${exists}`);
});
```

**Exempelutskrift** när mappen finns:
```
Mappen finns: true
```

Och om den inte gör det:
```
Mappen finns: false
```
