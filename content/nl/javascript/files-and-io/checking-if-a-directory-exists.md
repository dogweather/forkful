---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:08.961002-07:00
description: "Controleren of een directory bestaat gaat over bevestigen of een map\
  \ aanwezig is op een gespecificeerd pad in het bestandssysteem. Programmeurs doen\
  \ dit\u2026"
lastmod: '2024-03-11T00:14:25.054857-06:00'
model: gpt-4-0125-preview
summary: "Controleren of een directory bestaat gaat over bevestigen of een map aanwezig\
  \ is op een gespecificeerd pad in het bestandssysteem. Programmeurs doen dit\u2026"
title: Controleren of een directory bestaat
---

{{< edit_this_page >}}

## Wat & Waarom?
Controleren of een directory bestaat gaat over bevestigen of een map aanwezig is op een gespecificeerd pad in het bestandssysteem. Programmeurs doen dit om fouten te voorkomen, zoals proberen te lezen van of te schrijven naar een directory die er niet is.

## Hoe:
In JavaScript (draaiend in een Node.js omgeving), is er een ingebouwde module genaamd `fs` die je kunt gebruiken om te controleren of een directory bestaat. Hier is een snel voorbeeld:

```javascript
const fs = require('fs');
const path = './pad/naar/directory';

fs.access(path, fs.constants.F_OK, (err) => {
    if (err) {
        console.error(`${path} bestaat niet`);
    } else {
        console.log(`${path} bestaat`);
    }
});
```

Voorbeelduitvoer:

```
./pad/naar/directory bestaat
```

Of gebruikmakend van de nieuwere `fs.promises` API met async/await:

```javascript
const fs = require('fs').promises;

async function controleerOfDirectoryBestaat(path) {
    try {
        await fs.access(path, fs.constants.F_OK);
        console.log(`${path} bestaat`);
    } catch {
        console.error(`${path} bestaat niet`);
    }
}

controleerOfDirectoryBestaat('./pad/naar/directory');
```

Voorbeelduitvoer:

```
./pad/naar/directory bestaat niet
```

## Diepgaand
Historisch gezien betekende het controleren op een bestand of directory het gebruik van `fs.stat` of `fs.existsSync`, maar deze hebben nadelen. `fs.stat` vereist extra logica om te bepalen of het pad een directory is, en `fs.existsSync` is synchroon, wat de event loop in Node.js kan blokkeren.

Een alternatief is het gebruik van de `fs.promises` API of async/await voor betere leesbaarheid en om je programma niet-blokkerend te houden.

Een implementatiedetail is dat `fs.access` alleen controleert op het bestaan, niet de leesbaarheid of schrijfbaarheid van de directory. Andere vlaggen kunnen worden gebruikt met `fs.access` om indien nodig naar die permissies te controleren.

## Zie Ook
- Node.js `fs` documentatie: [Node.js fs module](https://nodejs.org/api/fs.html)
- Meer over async/await: [Asynchrone functie](https://developer.mozilla.org/nl/docs/Web/JavaScript/Reference/Statements/async_function)
- Info over bestandssysteemvlaggen: [Bestandssysteemvlaggen](https://nodejs.org/api/fs.html#file-system-flags)
