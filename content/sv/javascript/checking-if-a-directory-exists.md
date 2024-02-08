---
title:                "Kontrollera om en katalog existerar"
aliases:
- sv/javascript/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:46.768749-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kontrollera om en katalog existerar"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en katalog finns i JavaScript är avgörande för filhanteringsuppgifter, vilket möjliggör för skript att verifiera katalogens närvaro innan de läser från eller skriver till den. Denna operation förhindrar fel och säkerställer en smidigare programkörning, särskilt i applikationer som dynamiskt hanterar filer eller kataloger baserat på användarinput eller externa datakällor.

## Hur:
I Node.js, eftersom JavaScript i sig inte har direkt tillgång till filsystemet, används vanligtvis `fs`-modulen för sådana operationer. Här är ett enkelt sätt att kontrollera om en katalog finns med `fs.existsSync()`:

```javascript
const fs = require('fs');

const directoryPath = './sample-directory';

// Kontrollera om katalogen finns
if (fs.existsSync(directoryPath)) {
  console.log('Katalogen finns.');
} else {
  console.log('Katalogen finns inte.');
}
```
**Exempelutmatning:**
```
Katalogen finns.
```
Eller, för ett icke-blockerande asynkront tillvägagångssätt, använd `fs.promises` med `async/await`:

```javascript
const fs = require('fs').promises;

async function checkDirectory(directoryPath) {
  try {
    await fs.access(directoryPath);
    console.log('Katalogen finns.');
  } catch (error) {
    console.log('Katalogen finns inte.');
  }
}

checkDirectory('./sample-directory');
```
**Exempelutmatning:**
```
Katalogen finns.
```

För projekt som gör stor användning av fil- och katalogoperationer, erbjuder `fs-extra`-paketet, en utvidgning av den inbyggda `fs`-modulen, bekväma ytterligare metoder. Så här kan du uppnå samma sak med `fs-extra`:

```javascript
const fs = require('fs-extra');

const directoryPath = './sample-directory';

// Kontrollera om katalogen finns
fs.pathExists(directoryPath)
  .then(exists => console.log(exists ? 'Katalogen finns.' : 'Katalogen finns inte.'))
  .catch(err => console.error(err));
```
**Exempelutmatning:**
```
Katalogen finns.
```

Denna metod möjliggör ren, lättläst kod som problemfritt integreras med moderna JavaScript-praxis.
