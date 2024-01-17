---
title:                "Sammenligning av to datoer"
html_title:           "TypeScript: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sammenligning av to datoer er en vanlig oppgave for programmerere når de arbeider med å håndtere datoer og tidsstempel i sine applikasjoner. Dette er fordi det er viktig å kunne sammenligne datoer for å kunne gjøre beslutninger basert på ulike tidsrammer eller for å sortere og filtrere data basert på dato.

## Hvordan:
TypeScript har innebygde funksjoner for å sammenligne datoer og tidsstempel. Her er et eksempel på hvordan du kan sammenligne to datoer og få ut et resultat basert på sammenligningen:

```TypeScript
let currentDate = new Date(); //Opprettelse av en variabel for nåværende dato
let futureDate = new Date('2021-12-31'); //Opprettelse av en variabel for fremtidig dato

if (currentDate < futureDate) { //Sjekker om nåværende dato er før fremtidig dato
    console.log('Nåværende dato er før fremtidig dato'); //Printer ut en melding hvis betingelsen er oppfylt
} else {
    console.log('Nåværende dato er etter fremtidig dato'); //Printer ut en melding hvis betingelsen ikke er oppfylt
}
```
Dette vil gi følgende output:
```
Nåværende dato er før fremtidig dato
```

## Dypdykk:
Sammenligning av datoer og tidsstempel har vært en viktig del av dataprogrammering siden begynnelsen av datamaskiner. Historisk sett har dette vært en kompleks oppgave på grunn av forskjellige måter å representere dato og tid på. En alternativ måte å sammenligne datoer på er ved bruk av timestamp, som er et nummer som representerer antall millisekunder siden 1. januar 1970. Det er også viktig å være oppmerksom på at når man sammenligner datoer, sammenlignes de ofte på nøyaktig samme tidspunkt, noe som ikke alltid er ønskelig. Derfor kan det være nyttig å bruke funksjoner som ```setHours()``` og ```setMinutes()``` for å justere datoer til ønsket tidspunkt før sammenligning.

## Se også:
- [JavaScript Date object](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [Comparing dates in TypeScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/compare)