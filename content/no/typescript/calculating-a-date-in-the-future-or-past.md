---
title:                "TypeScript: Å beregne en dato i fremtiden eller fortiden"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor
Det er mange situasjoner hvor man kan ha behov for å beregne en dato i fremtiden eller fortiden. For eksempel når man planlegger en ferie eller skal sende ut viktige dokumenter med en bestemt frist. Ved å lære å beregne datoer i TypeScript, kan du lett automatisere denne prosessen og spare tid og energi.

## Slik gjør du det

For å beregne en dato i TypeScript, må du først importere Date-objektet fra standard biblioteket og deretter opprette et nytt Date-objekt med den ønskede datoen. Deretter kan du bruke forskjellige metoder og eigenskapar på dette objektet for å manipulere og hente ut den ønskede datoen.

For å beregne en dato i fremtiden, kan du bruke metoden `setDate()` for å sette datoen til et bestemt antall dager etter dagens dato. For eksempel:

```
TypeScript

// Opprett et nytt Date-objekt
const dato = new Date();

// Sett datoen til 5 dager etter dagens dato
dato.setDate(dato.getDate() + 5);

// Skriv ut datoen i formatet dd.mm.yyyy
console.log(`${dato.getDate()}.${dato.getMonth() + 1}.${dato.getFullYear()}`);

// Output: 09.07.2021
```

På samme måte kan du også beregne en dato i fortiden ved å bruke metoden `setDate()` og trekke fra et bestemt antall dager.

## Dypdykk
Å beregne datoer i TypeScript kan også gjøres ved hjelp av andre metoder og egenskaper på Date-objektet. For eksempel kan du bruke `setMonth()` for å sette måneden, `setFullYear()` for å sette året, og `setHours()` for å sette timene på en datodato. Du kan også bruke egenskapen `getDay()` for å hente ut hvilken dag i uken datoen vil falle på.

En viktig ting å huske på er at månedene i JavaScript starter fra 0 (0 for januar, 1 for februar osv.), mens datoer starter fra 1. Derfor må du legge til 1 når du skriver ut måneden ved hjelp av `getMonth()`.

I tillegg kan du også bruke andre metoder og biblioteker som Moment.js for mer avanserte datoberegninger.

## Se også
- [MDN dokumentasjon om JavaScript date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js dokumentasjon](https://momentjs.com/docs/)
- [TypeScript offisiell dokumentasjon](https://www.typescriptlang.org/docs/home.html)