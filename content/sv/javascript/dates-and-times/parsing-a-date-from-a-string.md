---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:29.993585-07:00
description: "Hur g\xF6r man: JavaScript erbjuder inbyggt `Date.parse()`-metoden och\
  \ `Date`-konstrukt\xF6ren f\xF6r att tolka datumstr\xE4ngar. Dock har dessa tillv\xE4\
  gag\xE5ngss\xE4tt\u2026"
lastmod: '2024-03-13T22:44:38.302665-06:00'
model: gpt-4-0125-preview
summary: "JavaScript erbjuder inbyggt `Date.parse()`-metoden och `Date`-konstrukt\xF6\
  ren f\xF6r att tolka datumstr\xE4ngar."
title: "Analysera ett datum fr\xE5n en str\xE4ng"
weight: 30
---

## Hur gör man:
JavaScript erbjuder inbyggt `Date.parse()`-metoden och `Date`-konstruktören för att tolka datumsträngar. Dock har dessa tillvägagångssätt begränsningar och inkonsekvenser över olika webbläsare, särskilt med icke-standardiserade datumformat. För att ta itu med dessa frågor är tredjepartsbibliotek som `Moment.js` och `date-fns` populära för deras robusthet och enkelhet i användning.

### Använda inbyggt JavaScript:
```javascript
const dateString = "2023-04-30T14:55:00";
const dateObj = new Date(dateString);

console.log(dateObj);  // Utmatning: Sun Apr 30 2023 14:55:00 GMT+0000 (Koordinerad universell tid)
```

### Använda Moment.js:
Först, installera Moment.js via npm eller inkludera det i ditt projekt. Sedan:
```javascript
const moment = require('moment');

const dateString = "2023-04-30T14:55:00";
const dateObj = moment(dateString);

console.log(dateObj.toString());  // Utmatning: Sun Apr 30 2023 14:55:00 GMT+0000
```

### Använda date-fns:
Efter att ha lagt till `date-fns` i ditt projekt, tolka en datumsträng på följande sätt:
```javascript
const { parseISO } = require('date-fns');

const dateString = "2023-04-30T14:55:00";
const dateObj = parseISO(dateString);

console.log(dateObj);  // Utmatning: 2023-04-30T14:55:00.000Z
```

Både `Moment.js` och `date-fns` erbjuder mer omfattande tolkningsförmåga, inklusive hantering av en mängd olika format och språkinställningar, vilket gör dem föredragna för komplexa applikationer.
