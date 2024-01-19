---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "TypeScript: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å beregne en dato i fremtiden eller fortiden er en sentral prosess for å finne ut nøyaktig hvilken dato det vil være i x antall dager, måneder eller år. Dette er nyttig for programmerere for å håndtere oppgaver som påminnelser, påloggingsdetaljer, og timeplanlegging.

## Hvordan gjøre det:
I TypeScript kan du beregne en dato i fremtiden eller fortiden ved hjelp av metoden `setDate()` som er en del av JavaScripts `Date` objekt.

Her kommer et eksempel:

```TypeScript
let dato: Date = new Date();  // Dagens dato
dato.setDate(dato.getDate() + 5);  // Legger til 5 dager til dagens dato
console.log(dato);  // Skriver ut den nye datoen
```

Når du kjører koden ovenfor, vil den skrive ut datoen 5 dager fra i dag.

## Deep Dive
Historisk sett har JavaScript en iboende `Date` objekt type som lar utviklere manipulere datoer. TypeScript, som er en overbygging av JavaScript, bruker også denne.

Alternativt kan du bruke biblioteker som Moment.js for mer komplekse dato- og tidsberegninger, men innebygd funksjonalitet fungerer for de fleste tilfeller.

Når det gjelder implementeringsdetaljer, bør du merke deg at `setDate()`-metoden endrer den opprinnelige datoen. Hvis du ønsker å beholde den opprinnelige datoen, dupliser den først.

## Se også
For mer informasjon om TypeScript og dato manipulasjon, se kildene nedenfor:

1. Mozilla Developer Network's guide på JavaScript Dato Objekter: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date

2. TypeScript sin offisielle dokumentasjon: https://www.typescriptlang.org/docs/

3. Detailert veiledning om hvordan du bruker Moment.js for dato og tid manipulasjon: https://momentjs.com/docs/