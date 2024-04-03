---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:02.549931-07:00
description: "Controleren of een directory bestaat gaat over zorgen dat een map daadwerkelijk\
  \ aanwezig is voordat je eruit leest of ernaar schrijft. Programmeurs doen\u2026"
lastmod: '2024-03-13T22:44:50.565763-06:00'
model: gpt-4-0125-preview
summary: Controleren of een directory bestaat gaat over zorgen dat een map daadwerkelijk
  aanwezig is voordat je eruit leest of ernaar schrijft.
title: Controleren of een directory bestaat
weight: 20
---

## Wat & Waarom?
Controleren of een directory bestaat gaat over zorgen dat een map daadwerkelijk aanwezig is voordat je eruit leest of ernaar schrijft. Programmeurs doen dit om fouten te voorkomen, zoals proberen een bestand op te slaan op een niet-bestaande plaats - dat is absoluut not done.

## Hoe te:
In TypeScript gebruik je meestal Node.js's `fs` module om te controleren op een directory. Hier is de snelle manier om het te doen:

```typescript
import { existsSync } from 'fs';

// Controleren of een directory bestaat
const directoryPath = './path/to/directory';

if (existsSync(directoryPath)) {
  console.log(`Yep, hij is er!`);
} else {
  console.log(`Nee, bestaat niet.`);
}
```

Output is afhankelijk van het bestaan van de directory:
```
Yep, hij is er!
// of
Nee, bestaat niet.
```

## Diepere Duik
Historisch gezien gebruikten mensen de asynchrone `fs.exists`, maar deze werd afgekeurd omdat het een vervelende gewoonte had om programmeerfouten te veroorzaken, zoals check-then-act racecondities. `existsSync` is eenvoudiger en schakelt de callback rommel uit.

Wat alternatieven betreft, de `fs.statSync` of `fs.accessSync` methoden kunnen ook het werk doen, maar vereisen wat meer code:

```typescript
import { statSync } from 'fs';

try {
  const stats = statSync(directoryPath);
  if (stats.isDirectory()) {
    console.log('Hij bestaat inderdaad.');
  }
} catch (error) {
  if (error.code === 'ENOENT') {
    console.log('Nee, nergens te vinden.');
  }
}
```

Zowel `statSync` als `accessSync` geven fouten als het pad niet bestaat, dus je moet dat afhandelen.

Wanneer je TypeScript gebruikt, herinner je dan dat deze methoden van Node.js komen, niet van TypeScript zelf. En de rol van TypeScript? Voornamelijk, het biedt de types en zorgt ervoor dat je de methoden correct gebruikt.

## Zie Ook
- Node.js Bestandssysteem Documentatie: https://nodejs.org/api/fs.html
- TypeScript Handboek: https://www.typescriptlang.org/docs/handbook/intro.html
- Foutafhandeling in Node.js: https://nodejs.org/en/knowledge/errors/what-are-the-error-conventions/
