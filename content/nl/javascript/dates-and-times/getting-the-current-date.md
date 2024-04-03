---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:06.052192-07:00
description: 'Hoe te: .'
lastmod: '2024-03-13T22:44:51.212782-06:00'
model: gpt-4-0125-preview
summary: .
title: Het huidige datum ophalen
weight: 29
---

## Hoe te:
```javascript
const now = new Date();
console.log(now.toString());  // Voorbeelduitvoer: Wo Apr 05 2023 20:46:28 GMT-0400 (Eastern Daylight Time)

console.log(now.toISOString());  // Voorbeelduitvoer: 2023-04-05T20:46:28.000Z
```

## Diep Duiken
Lang geleden werd het `Date` object in JavaScript ontwikkeld om data en tijden te beheren. Een `Date` object vertegenwoordigt een enkel moment in tijd, tot op de milliseconde.

**Alternatieven:**
- Bibliotheken zoals Moment.js (hoewel het nu als verouderd wordt beschouwd), date-fns, of Luxon kunnen meer functies bieden.
- Met Node.js kunt u ingebouwde modules voor tijd gebruiken, maar in de meeste gevallen werkt het native `Date` object prima.

**Implementatiedetails:**
- `Date` kan omgezet worden in een string of een specifiek formaat met behulp van methoden zoals `.toString(), .toISOString()`.
- Timezone eigenaardigheden zijn vaak pijnlijke punten. Let op, `.toISOString()` retourneert UTC-tijd.
- JavaScript telt tijd als milliseconden sinds het Unix Epoch (1 januari 1970, 00:00:00 UTC). Dit kun je krijgen met `Date.now()`.

## Zie Ook
- [MDN Web Docs over Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Je Hebt Moment.js Niet Nodig](https://you-dont-need.github.io/You-Dont-Need-Momentjs/)
- [Luxon Documentatie](https://moment.github.io/luxon/)
