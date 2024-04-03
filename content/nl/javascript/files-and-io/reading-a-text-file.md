---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:26.355589-07:00
description: 'Hoe: Hier is hoe je een tekstbestand leest in huidig JavaScript: **Met
  Node.js met Promises (Async/Await)**.'
lastmod: '2024-03-13T22:44:51.220091-06:00'
model: gpt-4-0125-preview
summary: Hier is hoe je een tekstbestand leest in huidig JavaScript.
title: Een tekstbestand lezen
weight: 22
---

## Hoe:
Hier is hoe je een tekstbestand leest in huidig JavaScript:

**Met Node.js met Promises (Async/Await)**:

```javascript
const fs = require('fs').promises;

async function readFile(filePath) {
  try {
    const data = await fs.readFile(filePath, 'utf8');
    console.log(data);
  } catch (error) {
    console.error('Kreeg een fout bij het proberen te lezen van het bestand:', error);
  }
}

readFile('voorbeeld.txt');
```

Voorbeelduitvoer (inhoud van `voorbeeld.txt`):

```
Hallo, dit is een tekstbestand!
```

**Met fetch API in de browser**:

```javascript
async function fetchTextFile(fileUrl) {
  try {
    const response = await fetch(fileUrl);
    const text = await response.text();
    console.log(text);
  } catch (error) {
    console.error('Oeps, er ging iets mis met het ophalen van het bestand:', error);
  }
}

fetchTextFile('voorbeeld.txt');
```

## Diepe Duik
Oorspronkelijk was het lezen van bestanden in JavaScript voornamelijk een server-side aangelegenheid, behandeld door Node.js. Toen JS zich met HTML5 een weg naar browsers baande, kwamen APIs zoals `FileReader` en `fetch` langs, waardoor het lezen van bestanden aan de client-side mogelijk werd zonder veel moeite.

Alternatieven? Oh, er zijn er een paar. Streams kunnen grote bestanden verwerken zonder het geheugen te belasten. Workers voorkomen dat de UI bevriest. Bibliotheken maken complexe taken gemakkelijker. Elk heeft zijn plaats.

Onder de motorkap kan het lezen van bestanden bufferbeheer, karaktercodering (UTF-8, enz.) en foutafhandeling omvatten. Wees ook bewust van veiligheid; browsers beperken bestandstoegang om goede redenen.

## Zie Ook
Breid je kennis verder uit met deze bronnen:

- MDN's FileReader API Doc: [MDN FileReader](https://developer.mozilla.org/en-US/docs/Web/API/FileReader)
- Node.js Bestandssysteem Documentatie: [Node.js fs](https://nodejs.org/api/fs.html)
- Stream API voor grote bestanden: [Node.js stream](https://nodejs.org/api/stream.html)
- Begrijpen van fetch API: [MDN fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
