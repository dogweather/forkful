---
title:                "TypeScript: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sammenligne to datoer kan være nyttig når du jobber med datoer i TypeScript-programmering. Det kan hjelpe deg med å bestemme hvilken dato som er tidligere, senere eller om de er like. Dette kan være nyttig for å filtrere eller sortere data, samt hjelpe deg med å lage logikk for å styre handlinger basert på datoer.

## Slik gjør du det

For å sammenligne to datoer i TypeScript, kan du bruke den innebygde `Date` klassen. Du kan opprette to datovariabler ved hjelp av `new Date()` metoden og deretter bruke sammenligningsoperatører som `<`, `>`, `<=` og `>=` for å sammenligne dem. Her er et eksempel på hvordan du kan gjøre det:

```TypeScript
let dato1 = new Date("2021-01-01");
let dato2 = new Date("2021-02-01");

if (dato1 < dato2) {
    console.log("Dato 1 kommer før dato 2");
}
```

I dette eksempelet vil konsollen skrive ut "Dato 1 kommer før dato 2" fordi februar (dato 2) kommer etter januar (dato 1).

## Dypdykk

I tillegg til de vanlige sammenligningsoperatørene, kan du også bruke `getTime()` metoden på `Date` objektet for å få datoen i millisekunder. Dette kan være nyttig hvis du trenger å sammenligne to nøyaktige tidspunkter. Du kan også bruke `getTime()` og `setTime()` metoder for å sammenligne og justere datoer ved hjelp av millisekunder. Her er et eksempel på hvordan du kan bruke dette:

```TypeScript
let dato1 = new Date("2021-01-01");
let dato2 = new Date("2021-01-02");

// Sammenligner datoen i millisekunder
if (dato1.getTime() < dato2.getTime()) {
    console.log("Dato 1 kommer før dato 2");
}

// Justerer dato 2 til å være en dag etter dato 1
dato2.setTime(dato1.getTime() + 86400000);
console.log(dato2); // Output: Sat Jan 02 2021 00:00:00 GMT+0100 (sentraleuropeisk normaltid)
```

Det er også verdt å merke seg at datoen i JavaScript og TypeScript er basert på UTC-tidssonen. Så når du sammenligner eller justerer datoer, vil det være viktig å ta hensyn til tilpasninger til lokale tidssoner.

## Se også

- [Date klasse i TypeScript dokumentasjon](https://www.typescriptlang.org/docs/handbook/standard-library.html#date)
- [Date objektet i JavaScript dokumentasjon](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)