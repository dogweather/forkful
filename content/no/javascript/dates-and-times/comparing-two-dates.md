---
date: 2024-01-20 17:33:25.243945-07:00
description: "Hvordan: I gamle dager brukte man ofte biblioteker som Moment.js for\
  \ datoh\xE5ndtering, men med tiden har JavaScript f\xE5tt bedre innebygd st\xF8\
  tte for dette.\u2026"
lastmod: '2024-04-05T21:53:42.155058-06:00'
model: gpt-4-1106-preview
summary: "I gamle dager brukte man ofte biblioteker som Moment.js for datoh\xE5ndtering,\
  \ men med tiden har JavaScript f\xE5tt bedre innebygd st\xF8tte for dette."
title: Sammenlikning av to datoer
weight: 27
---

## Hvordan:
```Javascript
const date1 = new Date('2023-03-01');
const date2 = new Date('2023-04-01');

// Sjekke om datoene er like
console.log(date1.getTime() === date2.getTime()); // false

// Finne ut hvilken dato som kommer først
console.log(date1 < date2 ? 'date1 er tidligere' : 'date2 er tidligere'); // date1 er tidligere

// Finne antall dager mellom to datoer
const diffTime = Math.abs(date2 - date1);
const diffDays = Math.ceil(diffTime / (1000 * 60 * 60 * 24)); 
console.log(diffDays + ' dager mellom datoene'); // 31 dager mellom datoene
```

## Dypdykk:
I gamle dager brukte man ofte biblioteker som Moment.js for datohåndtering, men med tiden har JavaScript fått bedre innebygd støtte for dette. Objektet `Date` er nå ganske robust og kan håndtere de fleste scenarier. 

Alternativer til `Date` inkluderer biblioteker som Day.js og date-fns for mer fleksibel og omfattende funksjonalitet, spesielt for internasjonale apps som trenger støtte for ulike tidssoner.

Når det gjelder implementasjon, så fungerer `Date`-objekter ved å lagre tidspunktet som et stort antall millisekunder siden midnatt UTC den 1. januar 1970. Metoden `getTime()` henter dette tallet, slik at sammenligning av datoer blir en sammenligning av to tall.

## Se Også:
- MDN Web Docs om `Date`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Day.js: https://day.js.org/
- date-fns: https://date-fns.org/
