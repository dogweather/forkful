---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:55.663560-07:00
description: "Hoe te: Laten we een tekstbestand in TypeScript lezen met behulp van\
  \ Node.js's `fs/promises` module. We houden dit voorbeeld simpel: lees een bestand\
  \ met\u2026"
lastmod: '2024-03-13T22:44:50.568664-06:00'
model: gpt-4-0125-preview
summary: Laten we een tekstbestand in TypeScript lezen met behulp van Node.js's `fs/promises`
  module.
title: Een tekstbestand lezen
weight: 22
---

## Hoe te:
Laten we een tekstbestand in TypeScript lezen met behulp van Node.js's `fs/promises` module. We houden dit voorbeeld simpel: lees een bestand met de naam `example.txt` en log de inhoud ervan.

```typescript
import { readFile } from 'fs/promises';

async function readTextFile(filePath: string) {
  try {
    const data = await readFile(filePath, 'utf8');
    console.log(data);
  } catch (error) {
    console.error(`Fout bij het lezen van het bestand van schijf: ${error}`);
  }
}

readTextFile('./example.txt');
```

Voorbeelduitvoer:
```
Hallo, dit is inhoud uit het bestand!
```

## Diepe Duik
Historisch gezien was het lezen van bestanden in Node.js sterk gebaseerd op callbacks, wat kon leiden tot een fenomeen dat bekend staat als "callback hel". Met de komst van Promises en `async/await` werd dit proces veel gestroomlijnder.

Naast `fs/promises`, is er ook de oudere `fs` module die nog steeds werkt met callbackpatronen. Er is ook de optie om streamverwerking te gebruiken met `fs.createReadStream()`, nuttig voor grote bestanden vanwege het lagere geheugenverbruik.

Implementatiegewijs is toegang tot het bestandssysteem een I/O-operatie en inherent trager dan operaties in het geheugen. Daarom zijn asynchrone programmeerpatronen belangrijk — ze helpen om het blokkeren van de hoofdthread te voorkomen en stellen Node.js in staat andere taken te blijven afhandelen.

## Zie Ook
Voor een diepere duik in het bestandssysteem van Node.js:
- Node.js fs documentatie: https://nodejs.org/api/fs.html
- Begrijpen van `fs/promises`: https://nodejs.org/dist/latest/docs/api/fs.html#filehandlepromises
- Streamgebaseerd bestandlezen: https://nodejs.org/api/stream.html#stream
Voor TypeScript specifieke bronnen:
- TypeScript Diepgaande Duik: https://basarat.gitbook.io/typescript/
- TypeScript Handboek: https://www.typescriptlang.org/docs/handbook/intro.html
