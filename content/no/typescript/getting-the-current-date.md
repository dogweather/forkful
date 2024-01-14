---
title:    "TypeScript: Å få gjeldende dato"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Hvorfor
Å forstå konseptet om å hente gjeldende dato kan være en nyttig ferdighet for enhver TypeScript-programmerer. Det gjør det mulig å vise riktig dato i applikasjoner og implementere ulike funksjoner som krever informasjon om tiden.

## Hvordan
Det første trinnet for å få den gjeldende datoen i TypeScript er å opprette et Date-objekt ved å bruke `new Date()`-konstruktøren. Dette vil opprette et objekt med informasjon om gjeldende dato og tidspunkt. Deretter kan vi bruke ulike metoder for å hente ut ønsket informasjon fra dette objektet.

```TypeScript
let currentDate = new Date();
console.log(currentDate); // Output: 2019-10-25T12:15:33.648Z

// Hente ut dato, måned og år
console.log(currentDate.getDate()); // Output: 25
console.log(currentDate.getMonth()); // Output: 10 (månedene er indeksert fra 0)
console.log(currentDate.getFullYear()); // Output: 2019

// Hente ut klokkeslett
console.log(currentDate.getHours()); // Output: 13 (1 på ettermiddagen)
console.log(currentDate.getMinutes()); // Output: 15
console.log(currentDate.getSeconds()); // Output: 33
```

Vi kan også formatere datoen basert på ønsket format ved å bruke `toLocaleDateString()`-metoden og sende inn parameter for ønsket språk. Dette vil returnere datoen som en lesbar streng.

```TypeScript
console.log(currentDate.toLocaleDateString("nb-NO")); // Output: 25.10.2019
```

## Dypdykk
Det er viktig å merke seg at JavaScript og TypeScript bruker den lokale datoen og tiden til enheten hvor koden blir kjørt. Dette kan føre til problemer hvis det er behov for å vise datoen i en spesifikk tidsone eller format. I slike tilfeller kan det være nødvendig å bruke biblioteker som Moment.js for å håndtere datoer og tider mer nøyaktig.

Det er også viktig å merke seg at `new Date()`-konstruktøren vil bruke tidssonen til enheten når den opprettes, noe som kan føre til uventede resultater når koden blir kjørt i forskjellige tidssoner. Ved å spesifisere en tidssone som parameter til `new Date()`-konstruktøren, kan vi få den gjeldende datoen i ønsket tidssone.

## Se også
- [Moment.js](https://momentjs.com/)
- [Date Object - MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)