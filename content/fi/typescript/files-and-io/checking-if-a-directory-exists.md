---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:53.087265-07:00
description: "Kuinka: TypeScript, kun sit\xE4 ajetaan Node.js-ymp\xE4rist\xF6ss\xE4\
  , mahdollistaa hakemiston olemassaolon tarkistamisen k\xE4ytt\xE4m\xE4ll\xE4 `fs`-moduulia,\
  \ joka tarjoaa\u2026"
lastmod: '2024-03-13T22:44:56.329363-06:00'
model: gpt-4-0125-preview
summary: "TypeScript, kun sit\xE4 ajetaan Node.js-ymp\xE4rist\xF6ss\xE4, mahdollistaa\
  \ hakemiston olemassaolon tarkistamisen k\xE4ytt\xE4m\xE4ll\xE4 `fs`-moduulia, joka\
  \ tarjoaa `existsSync()`-funktion tai asynkronisen `access()`-funktion yhdistettyn\xE4\
  \ `constants.F_OK`:n kanssa."
title: Tarkistetaan, onko hakemisto olemassa
weight: 20
---

## Kuinka:
TypeScript, kun sitä ajetaan Node.js-ympäristössä, mahdollistaa hakemiston olemassaolon tarkistamisen käyttämällä `fs`-moduulia, joka tarjoaa `existsSync()`-funktion tai asynkronisen `access()`-funktion yhdistettynä `constants.F_OK`:n kanssa.

### Käyttäen `fs.existsSync()`:
```typescript
import { existsSync } from 'fs';

const directoryPath = './path/to/directory';

if (existsSync(directoryPath)) {
  console.log('Hakemisto on olemassa.');
} else {
  console.log('Hakemistoa ei ole olemassa.');
}
```

### Käyttäen `fs.access()` yhdessä `fs.constants.F_OK` kanssa:
```typescript
import { access, constants } from 'fs';

const directoryPath = './path/to/directory';

access(directoryPath, constants.F_OK, (err) => {
  if (err) {
    console.log('Hakemistoa ei ole olemassa.');
    return;
  }
  console.log('Hakemisto on olemassa.');
});
```

**Esimerkkituloste** molemmille metodeille, olettaen että hakemisto on olemassa:
```
Hakemisto on olemassa.
```

Ja jos sitä ei ole:
```
Hakemistoa ei ole olemassa.
```

### Käyttäen kolmannen osapuolen kirjastoa - `fs-extra`:
`fs-extra` on suosittu kolmannen osapuolen kirjasto, joka parantaa sisäänrakennettua `fs`-moduulia ja tarjoaa kätevämpiä funktioita.

```typescript
import { pathExists } from 'fs-extra';

const directoryPath = './path/to/directory';

pathExists(directoryPath).then(exists => {
  console.log(`Hakemisto on olemassa: ${exists}`);
});
```

**Esimerkkituloste** kun hakemisto on olemassa:
```
Hakemisto on olemassa: true
```

Ja jos sitä ei ole:
```
Hakemisto on olemassa: false
```
