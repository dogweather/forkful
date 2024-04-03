---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:45.957614-07:00
description: "Hvordan: TypeScript, n\xE5r det kj\xF8res i et Node.js-milj\xF8, lar\
  \ deg sjekke om en mappe eksisterer ved \xE5 bruke `fs`-modulen, som gir `existsSync()`-funksjonen\u2026"
lastmod: '2024-03-13T22:44:40.546587-06:00'
model: gpt-4-0125-preview
summary: "TypeScript, n\xE5r det kj\xF8res i et Node.js-milj\xF8, lar deg sjekke om\
  \ en mappe eksisterer ved \xE5 bruke `fs`-modulen, som gir `existsSync()`-funksjonen\
  \ eller den asynkrone `access()`-funksjonen kombinert med `constants.F_OK`."
title: Sjekker om en mappe eksisterer
weight: 20
---

## Hvordan:
TypeScript, når det kjøres i et Node.js-miljø, lar deg sjekke om en mappe eksisterer ved å bruke `fs`-modulen, som gir `existsSync()`-funksjonen eller den asynkrone `access()`-funksjonen kombinert med `constants.F_OK`.

### Bruke `fs.existsSync()`:
```typescript
import { existsSync } from 'fs';

const directoryPath = './path/to/directory';

if (existsSync(directoryPath)) {
  console.log('Mappen eksisterer.');
} else {
  console.log('Mappen eksisterer ikke.');
}
```

### Bruke `fs.access()` med `fs.constants.F_OK`:
```typescript
import { access, constants } from 'fs';

const directoryPath = './path/to/directory';

access(directoryPath, constants.F_OK, (err) => {
  if (err) {
    console.log('Mappen eksisterer ikke.');
    return;
  }
  console.log('Mappen eksisterer.');
});
```

**Eksempel på utdata** for begge metodene, under antagelse av at mappen eksisterer:
```
Mappen eksisterer.
```

Og hvis den ikke gjør det:
```
Mappen eksisterer ikke.
```

### Bruke et bibliotek fra tredjepart - `fs-extra`:
`fs-extra` er et populært tredjeparts bibliotek som forbedrer det innebygde `fs`-modulen og gir mer bekvemme funksjoner.

```typescript
import { pathExists } from 'fs-extra';

const directoryPath = './path/to/directory';

pathExists(directoryPath).then(exists => {
  console.log(`Mappen eksisterer: ${exists}`);
});
```

**Eksempel på utdata** når mappen eksisterer:
```
Mappen eksisterer: true
```

Og hvis den ikke gjør det:
```
Mappen eksisterer: false
```
