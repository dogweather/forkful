---
title:                "Beregning av dato i fremtiden eller fortiden"
html_title:           "TypeScript: Beregning av dato i fremtiden eller fortiden"
simple_title:         "Beregning av dato i fremtiden eller fortiden"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kunne beregne en dato i fremtiden eller fortiden er en nyttig ferdighet som kan hjelpe oss med å planlegge og organisere tidsrelaterte oppgaver og hendelser. Ved å bruke TypeScript kan vi enkelt skrive kode som gjør dette for oss på en nøyaktig og effektiv måte.

TypeScript er et populært programmeringsspråk som brukes til å utvikle moderne webapplikasjoner. Det er basert på JavaScript, men legger til flere funksjoner og muligheter for å gjøre koden vår mer stabile og skalerbare.

## Hvordan

For å kunne beregne en dato i TypeScript, må vi først importere Date-objektet og konvertere datoen vi ønsker å beregne til millisekunder ved hjelp av metoden `.getTime()`. Deretter kan vi enten legge til eller trekke fra antall millisekunder vi ønsker å beregne fra den opprinnelige datoen og konvertere resultatet tilbake til en lesbar dato ved hjelp av `.toString()`-metoden.

```TypeScript
let startDato: Date = new Date("2021-04-15"); // Opprinnelig dato
let millisekunder: number = startDato.getTime(); // Konverterer til millisekunder
let antallDager: number = 10; // Antall dager vi ønsker å beregne

// Beregner sluttdato ved å legge til antall dager i millisekunder til startdato
let sluttDato: Date = new Date(millisekunder + (antallDager * 24 * 60 * 60 * 1000)); 
console.log(sluttDato.toString()); // "Sun Apr 25 2021"

// Beregner en dato i fortiden ved å trekke fra antall dager i millisekunder fra startdato
let forrigeDato: Date = new Date(millisekunder - (antallDager * 24 * 60 * 60 * 1000)); 
console.log(forrigeDato.toString()); // "Sat Apr 05 2021"
```

Det er viktig å merke seg at metoden for å konvertere mellom datotypene kan variere avhengig av hvilken formatering og output du ønsker. Du kan også beregne andre tidsenheter som timer, minutter og sekunder ved å gjøre endringer i utregningen med millisekunder.

## Dypdykk

Å kunne beregne datoer i TypeScript er nyttig, men det er også viktig å forstå hvordan datotypene og metoden `.getTime()` fungerer. Datoer i TypeScript er representert som antall millisekunder siden starten av 1970, også kjent som "epoch time". Dette gjør det mulig å utføre enkle matematiske operasjoner for å beregne datoer i fremtiden eller fortiden.

En annen viktig ting å merke seg er at metoden `.getTime()` også tar hensyn til tidssoner, så det kan være lurt å konvertere datoen til UTC-tid først for å unngå feil i utregningen.

## Se også

- [Datoer i TypeScript](https://www.typescriptlang.org/docs/handbook/declaration-files/do-s-and-don-ts.html#classes)
- [Manipulere datoer i TypeScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript-dokumentasjon](https://www.typescriptlang.org/docs/)