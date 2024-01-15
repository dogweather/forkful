---
title:                "Å få dagens dato"
html_title:           "Javascript: Å få dagens dato"
simple_title:         "Å få dagens dato"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å kunne hente den nåværende datoen er essensielt for å kunne lage dynamiske og interaktive nettsider og applikasjoner. Datoer brukes ofte til å vise informasjon som er relevant for brukeren, som for eksempel oppdateringer eller frister.

## Hvordan få den nåværende datoen

For å få den nåværende datoen i Javascript, kan du bruke funksjonen `new Date()`. Her er et eksempel på hvordan du kan bruke den til å vise dagens dato:

```javascript
let now = new Date();
console.log(now);
```

Output:

```
Thu Jan 09 2020 20:30:39 GMT+0100 (Central European Standard Time)
```

Du kan også formatere datoen på ulike måter ved å bruke forskjellige funksjoner og metoder. Her er et eksempel på hvordan du kan få datoen i en mer leselig form:

```javascript
let now = new Date();
let day = now.getDate();
let month = now.getMonth() + 1; // getMonth() gir måneden som et tall mellom 0-11, derfor legger vi til +1 for å få riktig måned
let year = now.getFullYear();
console.log(`${day}.${month}.${year}`);
```

Output:

```
9.1.2020
```

Det finnes også mange ferdige metoder og biblioteker som kan hjelpe deg med å formatere og håndtere datoer på en mer avansert måte.

## Dypdykk

I Javascript er datoen representert av antall millisekunder som har gått siden 1. januar 1970 klokken 00:00:00 UTC. Dette tilsvarer i utgangspunktet Unix-tidsstempelet. Hvis du ønsker å få tilgang til bestemte deler av datoen, som for eksempel bare tidsdelen eller måneden, finnes det også funksjoner og metoder for dette. Du kan lese mer om disse på [MDN dokumentasjonen](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date).

## Se også

- [MDN dokumentasjon om Date objektet](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js - et populært bibliotek for å håndtere datoer](https://momentjs.com/)
- [How to work with dates and times in JavaScript](https://www.hongkiat.com/blog/working-with-dates/)