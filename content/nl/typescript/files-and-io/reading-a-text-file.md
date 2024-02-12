---
title:                "Een tekstbestand lezen"
aliases:
- /nl/typescript/reading-a-text-file.md
date:                  2024-01-28T22:04:55.663560-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tekstbestand lezen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/typescript/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een tekstbestand lezen is het ophalen van de inhoud van een bestand dat is gestructureerd als leesbare tekst voor mensen. Programmeurs doen dit om de gegevens te verwerken of te analyseren, zoals het lezen van configuratie, het importeren van gegevens, of simpelweg het opnemen van inhoud om verwerkt te worden door een applicatie.

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
