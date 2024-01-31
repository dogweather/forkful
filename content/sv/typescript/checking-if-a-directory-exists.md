---
title:                "Kontrollera om en katalog finns"
date:                  2024-01-20T14:58:48.142757-07:00
simple_title:         "Kontrollera om en katalog finns"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Att kontrollera om en mapp finns handlar om att se till att en angiven sökväg leder till en existerande mapp innan man jobbar med den. Programmerare gör det för att undvika fel när filsysteminteraktioner kräver att mappen finns.

## How to:
Använd `fs`-modulen för att kontrollera om en mapp finns. Jag visar standard och asynkront sätt.

```TypeScript
import * as fs from 'fs';

// Synkront exempel
const dirPath: string = 'path/to/directory';

if (fs.existsSync(dirPath)) {
  console.log('Mappen finns!');
} else {
  console.log('Mappen finns inte.');
}

// Asynkront exempel med Promises
import { promises as fsPromises } from 'fs';

const checkDirectoryExists = async (path: string): Promise<void> => {
  try {
    await fsPromises.access(path, fs.constants.F_OK);
    console.log('Mappen finns!');
  } catch {
    console.log('Mappen finns inte.');
  }
}

checkDirectoryExists(dirPath);
```

Sample output:

```
Mappen finns!
```

eller

```
Mappen finns inte.
```

## Deep Dive
Förr använde vi `fs.exists`, men den är inaktuell på grund av dess oklara callback-hantering. `fs.existsSync` och `fs.promises.access` är pålitligare. Asynkron kontroll med `fs.promises.access` är att föredra i I/O-intensiva applikationer då den inte blockerar event-loopen.

Att använda `constants.F_OK` med `fs.promises.access` kontrollerar filsystemets tillgänglighet och är ett robust sätt att försäkra sig om mappens existens.

## See Also
- Node.js fs Documentation: https://nodejs.org/api/fs.html
- TypeScript Handbook: https://www.typescriptlang.org/docs/handbook/intro.html
- Async/Await i TypeScript: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-1.html
