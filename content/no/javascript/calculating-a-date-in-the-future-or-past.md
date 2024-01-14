---
title:                "Javascript: Å beregne en dato i fremtiden eller fortiden"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor
Å beregne en dato i fremtiden eller fortiden kan være nyttig for å planlegge hendelser eller for å utføre nødvendige beregninger. Dette kan være spesielt nyttig innen programmering, der mange systemer og applikasjoner krever at datoer blir håndtert på en nøyaktig og effektiv måte. Ved å lære å beregne datoer i JavaScript kan du forbedre dine programmeringsferdigheter og løse problemer relatert til tid og dato på en enkel måte.

## Slik gjør du det
For å beregne en dato i fortiden eller fremtiden i JavaScript, må du først starte med en grunnleggende forståelse av datotypen "Date". Dette kan gjøres ved å bruke "new Date()" -konstruktøren som vil opprette et Date-objekt med den aktuelle datoen og klokkeslettet til brukeren.

For å beregne en dato i fremtiden, kan du bruke "setDate()" -funksjonen for å legge til et visst antall dager på den nåværende datoen. Her er et eksempel på et kodestykke som beregner en dato 7 dager frem i tid:

```Javascript
var currentDate = new Date(); // Oppretter et Date-objekt med nåværende dato
var futureDate = currentDate.setDate(currentDate.getDate() + 7); // Beregner en dato 7 dager frem i tid
console.log(new Date(futureDate)); // Printer ut datoen i konsollen
```

Output vil være: Sun Jul 12 2020 16:10:55 GMT+0200 (sentraleuropeisk sommertid)

For å beregne en dato i fortiden, bruk "setDate()" -funksjonen igjen, men denne gangen kan du trekke fra et visst antall dager fra den nåværende datoen. Her er et eksempel på et kodestykke som beregner en dato 14 dager tilbake i tid:

```Javascript
var currentDate = new Date(); // Oppretter et Date-objekt med nåværende dato
var pastDate = currentDate.setDate(currentDate.getDate() - 14); // Beregner en dato 14 dager tilbake i tid
console.log(new Date(pastDate)); // Printer ut datoen i konsollen
```

Output vil være: Sat Jun 13 2020 16:10:55 GMT+0200 (sentraleuropeisk sommertid)

Det er også mulig å beregne datoer basert på måneder og år ved å bruke "setMonth()" og "setFullYear()" -funksjonene. Det er viktig å merke seg at måneder i JavaScript starter på 0 for januar og slutter på 11 for desember.

## Dypdykk
Det finnes også flere metoder og funksjoner for å håndtere datoer i JavaScript, som "getDay()" for å få ut hvilken ukedag en dato faller på. Det kan være nyttig å dykke dypere inn i dokumentasjonen til "Date" -objektet for å lære mer om alle de forskjellige mulighetene og funksjonene som er tilgjengelige.

Å beregne datoer i fremtiden eller fortiden kan også være nyttig for å håndtere omfattende tidsberegningsoppgaver, som å sjekke for overlapping mellom datoer eller beregne gjennomsnittlig tidsbruk. Dette er bare noen eksempler på bruksområder for å beregne datoer i JavaScript, og mulighetene er mange.

## Se også
- [Date Object in JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Date Manipulation in JavaScript](https://www.w3schools.com/js/js_dates.asp)
- [Understanding Date in JavaScript](https://www.digitalocean.com/community/tutorials/understanding-date-and-time-in-javascript)