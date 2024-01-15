---
title:                "Få gjeldende dato"
html_title:           "TypeScript: Få gjeldende dato"
simple_title:         "Få gjeldende dato"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Før vi hopper inn i hvordan man kan få tak i dagens dato ved hjelp av TypeScript, la oss først se på hvorfor dette er nyttig. Å få tak i dagens dato er en vanlig oppgave i mange programmeringsprosjekter, spesielt når man jobber med tidsstyring og planlegging. Å kunne få tak i den nåværende datoen kan være nyttig for å holde styr på deadlines, opprette datoen for en transaksjon eller for å vise den nåværende datoen til brukere.

## Hvordan man får tak i dagens dato

For å få tak i dagens dato ved hjelp av TypeScript, kan vi bruke det innebygde Date-objektet. Det er flere metoder som kan brukes til å hente ut informasjon om dagens dato, for eksempel `getDate()`, `getMonth()` og `getFullYear()`.

Her er et eksempel på hvordan man kan bruke Date-objektet til å få tak i dagens dato og skrive den ut i konsollen:

```TypeScript
let currentDate = new Date(); // Oppretter et Date-objekt med dagens dato
console.log(currentDate); // Skriver ut dato-objektet i konsollen
```

Sample output:
`2021-06-22T08:54:55.298Z`

Vi kan også formatere utskriften ved hjelp av `getMonth()`, `getDate()` og `getFullYear()`-metodene:

```TypeScript
let month = currentDate.getMonth() + 1; // Legger til 1, siden måneder starter på 0
let day = currentDate.getDate();
let year = currentDate.getFullYear();
console.log(`I dag er det ${day}.${month}.${year}`); // Skriver ut datoen i formatet DD.MM.ÅÅÅÅ
```

Sample output:
`I dag er det 22.6.2021`

## Dypdykk

Date-objektet i TypeScript støtter også en rekke andre nyttige metoder for å håndtere datoer og tid, for eksempel `toLocaleDateString()`, `toLocaleTimeString()` og `toLocaleString()`. Disse metodene lar deg formatere dato og klokkeslett basert på brukerens lokale innstillinger.

Det finnes også biblioteker som Moment.js som gir en mer robust løsning for dato og tid-manipulasjon i TypeScript. Dette kan være nyttig hvis du trenger å håndtere kompliserte datofunksjoner i prosjektet ditt.

## Se også

- [Date - MDN web docs](https://developer.mozilla.org/nb/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)