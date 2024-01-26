---
title:                "Slik får du tak i dagens dato"
date:                  2024-01-20T15:16:56.148048-07:00
html_title:           "C: Slik får du tak i dagens dato"
simple_title:         "Slik får du tak i dagens dato"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å hente dagens dato betyr å få nøyaktig tidspunkt for nåværende øyeblikk fra systemet. Programmerere gjør dette for å skape tidspunktavhengig funksjonalitet som logger, tidsstempler eller tidsfrister.

## Hvordan:
```TypeScript
// Få dagens dato som et Date-objekt
const nå = new Date();
console.log(nå); // Logger noe ala: 2023-04-01T12:00:00.000Z

// Få lesbar string-format
console.log(nå.toDateString()); // Logger noe ala: Sat Apr 01 2023

// Få lokal tid som string
console.log(nå.toLocaleTimeString()); // Logger noe ala: 12:00:00 PM
```

## Dypdykk
I JavaScript, og dermed også i TypeScript, representeres datoer med `Date` objektet, introdusert i ECMAScript 1. Alternativer til å bruke `Date` inkluderer biblioteker som `moment.js` eller `date-fns` som tilbyr mer funksjonalitet og enklere håndtering av dato og tid. Implementasjonen av `Date` i JavaScript håndterer tid i millisekunder siden Unix epoch (1. januar 1970 00:00:00 UTC), og kan konvertere til forskjellige formater for enkel visning og bruk.

## Se Også:
- MDN Web Docs for `Date`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- moment.js: https://momentjs.com/
- date-fns: https://date-fns.org/
- ECMAScript Spesifikasjon for `Date`: https://tc39.es/ecma262/#sec-date-objects
