---
title:                "Tolke en dato fra en streng"
date:                  2024-01-20T15:39:01.916041-07:00
html_title:           "Arduino: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av dato fra en streng tolker teksten til et datoobjekt. Vi gjør det for å enkelt kunne manipulere og sammenligne datoer i kode.

## How to:
```TypeScript
// Bruk av innebygd Date konstruktør
const dateString: string = "2023-04-05T14:23:00.000Z";
const date: Date = new Date(dateString);
console.log(date.toString()); // Skriver ut datoen i lokaltid

// Med Date.parse (returnerer en number timestamp)
const timestamp: number = Date.parse(dateString);
console.log(new Date(timestamp).toString()); // Konverterer timestamp tilbake til Date og skriver ut

// Ved bruk av biblioteket 'date-fns'
import { parseISO } from 'date-fns';
const dateWithLibrary: Date = parseISO(dateString);
console.log(dateWithLibrary.toString()); // Skriver ut datoen
```

## Deep Dive
Historisk sett har JavaScript og TypeScript håndtert datoer med Date-objektet. Det har mottatt kritikk for tidszonespørsmål og inkonsistente resultater. Biblioteker som `moment.js` og `date-fns` gir bedre parsing og formatering.

Når vi parser en dato fra en streng, bør vi være oppmerksom på ISO 8601-formatet (`yyyy-mm-ddTHH:MM:ss.sssZ`), som er standarden Date-konstruktøren forventer. Noen ganger må man håndtere forskjellige datoformat, og da kan biblioteker som `date-fns` eller `moment.js` være til hjelp.

Implementeringsdetaljer som viktig:
- Tidszoner: Vær klar over tidszonen strengen representerer, og hvordan det håndteres i datotypen du jobber med.
- Feilhåndtering: Hva skjer om strengen ikke er en gyldig dato? Sørg for å ha kontroll på dette.

Alternativer til innebygde metoder inkluderer tredjepartsbiblioteker som `moment.js` (som nå er i vedlikeholdsmodus, så nye prosjekter anbefales å bruke noe annet), `date-fns`, og `Day.js`. Disse har ofte mer funksjonalitet og bedre global støtte.

## See Also
- [MDN Web Docs: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [date-fns: Modern JavaScript date utility library](https://date-fns.org/)
- [ISO 8601 Date and time format](https://www.iso.org/iso-8601-date-and-time-format.html)