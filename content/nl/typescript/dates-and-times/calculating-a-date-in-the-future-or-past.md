---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:01.807548-07:00
description: "Hoe te: Historisch gezien is het beheren van datums in JavaScript\u2014\
  en bij uitbreiding TypeScript\u2014lastig geweest vanwege de eigenaardigheden van\
  \ het Date-\u2026"
lastmod: '2024-04-05T22:51:03.402418-06:00'
model: gpt-4-0125-preview
summary: "Historisch gezien is het beheren van datums in JavaScript\u2014en bij uitbreiding\
  \ TypeScript\u2014lastig geweest vanwege de eigenaardigheden van het Date-object\
  \ en tijdzones."
title: Een datum in de toekomst of het verleden berekenen
weight: 26
---

## Hoe te:
```TypeScript
// Huidige datum ophalen
const today: Date = new Date();

// 10 dagen in de toekomst berekenen
const tenDaysLater: Date = new Date(today.getTime() + (10 * 24 * 60 * 60 * 1000));
console.log(`Over tien dagen: ${tenDaysLater.toDateString()}`);

// 10 dagen in het verleden berekenen
const tenDaysBefore: Date = new Date(today.getTime() - (10 * 24 * 60 * 60 * 1000));
console.log(`Tien dagen geleden was: ${tenDaysBefore.toDateString()}`);
```
Voorbeelduitvoer:
```
Over tien dagen: Zon Apr 23 2023
Tien dagen geleden was: Woe Apr 03 2023
```

## Uitdieping
Historisch gezien is het beheren van datums in JavaScript—en bij uitbreiding TypeScript—lastig geweest vanwege de eigenaardigheden van het Date-object en tijdzones. Alternatieve bibliotheken zoals Moment.js en date-fns hebben abstracties geboden om deze complexiteit te hanteren. Met ES6 kwam betere ondersteuning voor internationalisatie via de `Intl` API, die TypeScript ook kan gebruiken.

Bij het berekenen van datums, let op veranderingen in de zomertijd en schrikkelseconden. Deze kunnen eenvoudige berekeningen zoals het toevoegen van 24 uur aan een datum verstoren. Overweeg ook altijd de locale en tijdzone van de gebruiker bij het weergeven van berekende datums.

Voor brede compatibiliteit en flexibiliteit zou je kunnen kiezen voor bibliotheken zoals `date-fns` of `Luxon`, die modulair zijn en geweldig voor complexe taken. Bijvoorbeeld, met `date-fns`, kun je gemakkelijk dagen toevoegen:

```TypeScript
import { addDays } from 'date-fns';

const result = addDays(new Date(2023, 3, 13), 10); // 13 april 2023 + 10 dagen
console.log(result.toDateString());
```

Ze behandelen ook randgevallen en tijdzoneproblemen, waardoor veel van de pijn van datumrekenkunde weggenomen wordt.

## Zie ook
- [MDN Datumreferentie](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [date-fns Bibliotheek](https://date-fns.org/)
- [Luxon Documentatie](https://moment.github.io/luxon/#/)
- [TypeScript Officiële Documentatie](https://www.typescriptlang.org/docs/)
