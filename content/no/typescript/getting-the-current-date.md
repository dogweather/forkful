---
title:                "Få dagens dato"
html_title:           "TypeScript: Få dagens dato"
simple_title:         "Få dagens dato"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å få tak i nåværende dato er en metode som lar programmerere få tak i informasjon om nåværende tid og dato. Dette er nyttig for å kunne spore og organisere data og for å legge til tidsbaserte funksjoner i applikasjoner.

## Hvordan:
Det å få tak i nåværende dato i TypeScript er raskt og enkelt med hjelp av den innebygde Date-klassen. Her er et eksempel på hvordan man får tak i nåværende dato og tid:

```TypeScript
let nåværendeDato = new Date();
console.log(nåværendeDato);
```

Dette vil gi følgende utskrift: 
```TypeScript
Fri Nov 15 2019 13:39:25 GMT+0100 (Sentraleuropeisk standardtid)
```

Dersom man ønsker å formatere utdaten på en spesifikk måte, kan man også bruke metoder på Date-klassen som `getDate()`, `getMonth()`, `getFullYear()` og `getHours()`, for å hente ut spesifikke deler av datoen og tiden.

## Dypdykk:
Datoer og tider har alltid vært viktige for programmerere, spesielt når det gjelder loggføring og dataorganisering. Før moderne programmeringsspråk, måtte programmerere skrive komplekse algoritmer for å få tak i nåværende dato og tid. Med TypeScript og Date-klassen, er dette nå enklere og mer effektivt.

Alternativet til å bruke Date-klassen i TypeScript vil være å bruke et tredjeparts bibliotek som Moment.js, som tilbyr flere funksjoner og formateringsmuligheter.

Når man bruker Date-klassen, er det viktig å være oppmerksom på at datoen og tiden som returneres, er basert på brukerens lokale tidssone. Dette kan føre til problemer når man jobber med internasjonale prosjekter.

## Se også:
- [Date-klassen i TypeScript](https://www.typescriptlang.org/docs/handbook/utilities.html)
- [Moment.js](https://momentjs.com/)