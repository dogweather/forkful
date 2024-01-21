---
title:                "Sammenlikning av to datoer"
date:                  2024-01-20T17:34:16.164284-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sammenlikning av to datoer"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Sammenligning av to datoer handler om å finne ut hvilken som kommer før eller etter, eller om de er like. Programmerere gjør det for å håndtere tidsavhengige funksjoner som utløpsdatoer, tidslinjer eller å sortere hendelser.

## How to: (Hvordan:)
```TypeScript
// Lag to datoobjekter
let date1 = new Date('2023-01-01');
let date2 = new Date('2023-12-31');

// Sammenlign datoer
if (date1 < date2) {
    console.log('Date1 kommer før Date2.');
} else if (date1 > date2) {
    console.log('Date1 kommer etter Date2.');
} else {
    console.log('Datoene er de samme.');
}

// Formater og sammenlign som strenger
let isoDate1 = date1.toISOString().substring(0, 10);
let isoDate2 = date2.toISOString().substring(0, 10);

if (isoDate1.localeCompare(isoDate2) < 0) {
    console.log('ISO Date1 kommer før ISO Date2.');
} else if (isoDate1.localeCompare(isoDate2) > 0) {
    console.log('ISO Date1 kommer etter ISO Date2.');
} else {
    console.log('ISO datoene er de samme.');
}
```

Sample output:
```
Date1 kommer før Date2.
ISO Date1 kommer før ISO Date2.
```

## Deep Dive (Dypdykk)
Å sammenligne datoer i TypeScript kan gjøres direkte fordi JavaScripts `Date`-objekter kan sammenlignes med vanlige operators som `<` og `>`. Dette er mulig siden `Date`-objekter internt representerer tidspunkter som antall millisekunder siden et referansepunkt (1. januar 1970 UTC).

Alternativt kan man bruke metoden `getTime()` for å hente det numeriske tidsstempelet og sammenligne disse verdiene. Eller, for lesbarhet, kan datoene konverteres til ISO-strenger og sammenlignes leksikografisk med `localeCompare`.

Det er verdt å merke seg at tidssoneforskjeller kan påvirke sammenligningsresultatet, og det bør tas hensyn til om applikasjonen er tidssone-sensitiv.

## See Also (Se Også)
- MDN Web Docs om `Date`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- TypeScript Handbook: https://www.typescriptlang.org/docs/handbook/intro.html
- Date-fns biblioteket, et moderne verktøysett for å jobbe med datoer: https://date-fns.org/
- Moment.js, et eldre, men fortsatt populært bibliotek for dato-manipulering: https://momentjs.com/