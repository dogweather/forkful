---
aliases:
- /nl/typescript/comparing-two-dates/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:56.807666-07:00
description: "Het vergelijken van twee datums houdt in dat je hun chronologische relatie\
  \ uitzoekt - zijn ze hetzelfde, is de ene eerder, of misschien later?\u2026"
lastmod: 2024-02-18 23:09:01.586719
model: gpt-4-0125-preview
summary: "Het vergelijken van twee datums houdt in dat je hun chronologische relatie\
  \ uitzoekt - zijn ze hetzelfde, is de ene eerder, of misschien later?\u2026"
title: Twee datums vergelijken
---

{{< edit_this_page >}}

## Wat & Waarom?

Het vergelijken van twee datums houdt in dat je hun chronologische relatie uitzoekt - zijn ze hetzelfde, is de ene eerder, of misschien later? Programmeurs doen dit om evenementen te plannen, tijdlijnen te sorteren en de duur te controleren.

## Hoe te:

Laten we enkele datums vergelijken:

```TypeScript
const date1 = new Date('2023-04-01T00:00:00Z');
const date2 = new Date('2023-04-02T00:00:00Z');

// Is date1 voor date2?
console.log(date1 < date2); // waar

// Is date1 hetzelfde als date2?
console.log(date1.getTime() === date2.getTime()); // onwaar

// Hoeveel dagen verschil?
const diffTime = Math.abs(date2.getTime() - date1.getTime());
const diffDays = Math.ceil(diffTime / (1000 * 60 * 60 * 24)); 
console.log(diffDays); // 1
```

Voorbeelduitvoer:

```
waar
onwaar
1
```

## Diepere Duik

Vroeger waren datums een hooiberg van formaten en door elkaar gehaalde berekeningen. Met JavaScript (en bij uitbreiding TypeScript), vereenvoudigde het `Date` object zaken, door te standaardiseren hoe we met tijd omgaan.

Alternatieven? Zeker. Bibliotheken zoals `moment.js` of `date-fns` vergroten de functionaliteit van datumbehandeling met extra functies. Maar voor basisvergelijkingen? De eenvoud van de native Date doet vaak het werk.

Achter de schermen krijgt `Date.getTime()` de milliseconden sinds het tijdperk (1 januari 1970). Het vergelijken van deze waarden elimineert eigenaardigheden van tijdzones en schrikkelseconden, en reduceert het tot nummers.

## Zie Ook

- [Mozilla Developer Network Datum Referentie](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date) voor de ins en outs van Date objecten.
- [You Don't Need Moment.js](https://github.com/you-dont-need/You-Dont-Need-Momentjs) voor de tijden dat je misschien wel of niet een bibliotheek wilt.
- [TypeScript Officiële Documentatie](https://www.typescriptlang.org/docs/) voor meer over de kracht en valkuilen van TypeScript.
