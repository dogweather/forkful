---
date: 2024-01-20 17:37:32.272548-07:00
description: "How to: Datoformat varierer over hele verden. Tidligere brukte programmerere\
  \ egne funksjoner for \xE5 h\xE5ndtere dette, men JavaScripts `Date`-objekt forenkler\u2026"
lastmod: '2024-04-05T21:53:41.521033-06:00'
model: gpt-4-1106-preview
summary: Datoformat varierer over hele verden.
title: Konvertere en dato til en streng
weight: 28
---

## How to:
```TypeScript
const currentDate: Date = new Date();
const dateString: string = currentDate.toISOString(); // Standard ISO-format
console.log(dateString); // 2023-03-11T16:20:00.000Z

// Enkel norsk datoformat
const norskDato: string = currentDate.toLocaleDateString('no-NO');
console.log(norskDato); // 11.03.2023

// Norsk dato og tid
const norskDatoTid: string = currentDate.toLocaleString('no-NO');
console.log(norskDatoTid); // 11.03.2023, 16:20:00
```

## Deep Dive
Datoformat varierer over hele verden. Tidligere brukte programmerere egne funksjoner for å håndtere dette, men JavaScripts `Date`-objekt forenkler prosessen betydelig. Med `Date.toISOString()` får vi et standardisert ISO-format, men for lokal bruk er `Date.toLocaleDateString()` og `Date.toLocaleString()` gull. Disse tar imot `locales` og `options` argumenter, så du kan tilpasse strengen til det norske formatet – eller et hvilket som helst annet språk og format.

Det finnes alternativer som biblioteker, for eksempel `moment.js` eller `date-fns`, som tilbyr enda mer kontroll og tilpasning, men for mange tilfeller er innebygde JavaScript-metoder tilstrekkelige.

Implementeringsdetaljer inkluderer behandling av tidssoner og sommertid. For eksempel, `.toISOString()` gir alltid UTC-tid, mens `.toLocaleString()` justerer til brukerens tidssone.

## See Also
- [MDN Web Docs: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [date-fns Documentation](https://date-fns.org/docs/Getting-Started)
- [moment.js Home](https://momentjs.com/)
