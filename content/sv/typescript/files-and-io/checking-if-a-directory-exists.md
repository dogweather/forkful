---
title:                "Kontrollera om en katalog existerar"
aliases:
- /sv/typescript/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:45.468259-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kontrollera om en katalog existerar"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en mapp finns i TypeScript är nödvändigt för filhanteringsuppgifter, såsom att läsa från eller skriva data till filer, för att säkerställa att operationer endast utförs på giltiga mappar. Denna operation är avgörande för att undvika fel som uppstår vid försök att komma åt eller manipulera icke-existerande mappar.

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
