---
title:                "Javascript: Hente nåværende dato."
simple_title:         "Hente nåværende dato."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

I dagens digitale verden er det å kunne håndtere og håndtere datoer og tidsstempel en avgjørende ferdighet for en programmerer. Å kunne få den nåværende datoen er viktig for å kunne vise riktig informasjon til brukere, trekke ut data fra en database basert på dato eller til og med sjekke hvor lenge siden en hendelse fant sted. I denne bloggposten vil vi utforske hvordan man kan få tak i den nåværende datoen ved hjelp av Javascript.

## Hvordan

Det er flere måter å få tak i den nåværende datoen på ved hjelp av Javascript. En av de enkleste måtene er ved hjelp av Date-objektet. Dette objektet representerer et bestemt tidspunkt og vi kan få tak i den nåværende datoen ved å bruke metoden `getDate()`.

```Javascript
const dato = new Date();
const nåværendeDato = dato.getDate();

console.log(nåværendeDato); // Output: 16 (hvis det er den 16. dagen i måneden)
```

Vi kan også formatere datoen ved hjelp av `toLocaleString()`-metoden for å få en mer lesbar utskrift av datoen.

```Javascript
const dato = new Date();
const options = { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' };
const nåværendeDato = dato.toLocaleString('no-NO', options);

console.log(nåværendeDato); // Output: tirsdag 16. juni 2020
```

Vi kan også få tilgang til individuelle deler av datoen, som dag, måned og år, ved hjelp av `getDay()`, `getMonth()` og `getFullYear()` metodene.

```Javascript
const dato = new Date();
const dag = dato.getDay();
const måned = dato.getMonth();
const år = dato.getFullYear();

console.log(dag); // Output: 2 (hvis det er tirsdag)
console.log(måned); // Output: 5 (hvis det er juni, da måneder i Javascript starter på 0)
console.log(år); // Output: 2020 (hvis det er 2020 nå)
```

## Dypdykk

Nå som vi har fått en grunnleggende forståelse av hvordan vi kan få tak i den nåværende datoen, la oss se på noen nyttige metoder som `setDate()` og `setMonth()` som lar oss endre datoen.

```Javascript
const dato = new Date();
dato.setDate(26); // Setter datoen til den 26.
dato.setMonth(8); // Setter måneden til september, siden måneder i Javascript starter på 0

console.log(dato.toLocaleString('no-NO')); // Output: lørdag 26. september 2020
```

Vi kan også bruke `getTime()`-metoden for å få antall millisekunder siden 1. januar 1970. Dette kan være nyttig hvis vi trenger å beregne tidsforskjeller mellom to datoer.

```Javascript
const nå = new Date();
const tidligere = new Date('June 10 2020');

const tidsforskjell = nå.getTime() - tidligere.getTime();
const dager = tidsforskjell / (1000 * 60 * 60 * 24);

console.log(dager); // Output: 6 (avhengig av hvilken dato det er nå)
```

## Se også

- [Date-objektet i Javascript](https://developer.mozilla.org/no/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Manipulere datoer i Javascript](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [Formatere datoer i Javascript](https://www.sitepoint.com/javascript-date-examples/)