---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:30.553724-07:00
description: "Het vergelijken van twee datums betekent controleren of ze hetzelfde\
  \ zijn of bepalen welke eerder of later komt. Programmeurs hebben dit vaak nodig\
  \ voor\u2026"
lastmod: '2024-03-13T22:44:51.214734-06:00'
model: gpt-4-0125-preview
summary: Het vergelijken van twee datums betekent controleren of ze hetzelfde zijn
  of bepalen welke eerder of later komt.
title: Twee datums vergelijken
weight: 27
---

## Wat & Waarom?

Het vergelijken van twee datums betekent controleren of ze hetzelfde zijn of bepalen welke eerder of later komt. Programmeurs hebben dit vaak nodig voor deadlines, het plannen van evenementen of gewoon voor het bijhouden van tijd.

## Hoe:

JavaScript's `Date` objecten komen goed van pas. Wanneer je ze vergelijkt, worden ze omgezet in milliseconden sinds 1 januari 1970, UTC.

```javascript
let date1 = new Date('2021-07-24');
let date2 = new Date('2021-07-25');

console.log(date1 < date2); // waar
console.log(date1 > date2); // onwaar
console.log(date1.getTime() === date2.getTime()); // onwaar
```

Voorbeeld output:

```
waar
onwaar
onwaar
```

## Diepere duik

Achter de schermen zijn `Date` objecten gewoon milliseconden. Historisch gezien moesten programmeurs datumoperaties handmatig beheren, de verstreken tijd berekenen vanaf een ijkpunt, waarbij ze vaak fouten riskeerden. Het vergelijken van `Date` objecten maakt het leven makkelijker, hoewel nog steeds niet foutvrij, vooral met tijdzones en zomertijd.

Alternatieven? Zeker. Bibliotheken zoals `moment.js` of `date-fns` helpen bij het omgaan met complexe scenario's en bieden extra gemakken voor datumanipulatie.

Wat betreft de implementatie, is het belangrijk te onthouden dat het direct vergelijken van `Date` objecten (met `==`) verwijzingen vergelijkt, niet waarden. Gebruik `getTime()` voor een accurate waardevergelijking. En let op tijdzones bij het parseren van datums; het is makkelijk om struikelblokken te vinden als je niet voorzichtig bent.

## Zie ook

- MDN-websites over Date: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js Bibliotheek: https://momentjs.com/
- date-fns Bibliotheek: https://date-fns.org/
