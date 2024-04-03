---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:00.558209-07:00
description: "Hoe: TypeScript, als een uitbreiding van JavaScript, heeft geen eigen\
  \ bestandssysteemmodule, maar het kan Node.js gebruiken voor deze taak. Zorg ervoor\u2026"
lastmod: '2024-03-13T22:44:50.569950-06:00'
model: gpt-4-0125-preview
summary: TypeScript, als een uitbreiding van JavaScript, heeft geen eigen bestandssysteemmodule,
  maar het kan Node.js gebruiken voor deze taak.
title: Een tekstbestand schrijven
weight: 24
---

## Hoe:
TypeScript, als een uitbreiding van JavaScript, heeft geen eigen bestandssysteemmodule, maar het kan Node.js gebruiken voor deze taak. Zorg ervoor dat je Node.js hebt geïnstalleerd en dan, laten we beginnen:

```typescript
// Importeer de 'fs'-module om te interageren met het bestandssysteem
importeer { writeFile } van 'fs';

// De inhoud die je wilt schrijven
const inhoud = 'Hallo, wereld!';

// Functie om inhoud naar een bestand te schrijven
const schrijfTekstNaarBestand = (bestandspad: string, inhoud: string): void => {
  writeFile(bestandspad, inhoud, (err) => {
    if (err) {
      console.error('Fout bij het schrijven van bestand:', err);
    } else {
      console.log('Bestand succesvol geschreven');
    }
  });
};

// Gebruik de functie om te schrijven naar 'output.txt'
schrijfTekstNaarBestand('./output.txt', inhoud);
```

Voorbeelduitvoer:
```
Bestand succesvol geschreven
```

## Diepe duik
Historisch gezien is het schrijven naar tekstbestanden zo oud als de computer zelf voor opslag of communicatie tussen programma's. Voordat databases algemeen werden gebruikt, waren platte bestanden gebruikelijk. Nu hebben databases grotendeels deze rol overgenomen, maar tekstbestanden zijn nog steeds van vitaal belang vanwege hun eenvoud.

Alternatieven voor de Node.js 'fs'-module zijn:

- De nieuwe 'fs/promises' voor Promise-gebaseerde functies.
- Het gebruik van 'fs-extra' voor handige methodes.
- 'stream'-module voor het omgaan met grote bestanden.

De 'writeFile'-methode die hierboven is getoond, werkt goed voor kleine tot middelgrote bestanden. Voor grotere bestanden of gegevensstromen wil je misschien streams gebruiken om te voorkomen dat alles in het geheugen wordt geladen.

## Zie ook
- Node.js Bestandssysteem API: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- Officiële pagina van TypeScript: [https://www.typescriptlang.org/](https://www.typescriptlang.org/)
- 'fs-extra' bibliotheek: [https://github.com/jprichardson/node-fs-extra](https://github.com/jprichardson/node-fs-extra)
- MDN Web Docs over Streams: [https://developer.mozilla.org/nl/docs/Web/API/Streams_API](https://developer.mozilla.org/nl/docs/Web/API/Streams_API)
