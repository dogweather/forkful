---
title:    "TypeScript: Sammenligner to datoer."
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

En viktig del av å programmere er å kunne sammenligne og manipulere datoer. Dette er spesielt nyttig når du jobber med forskjellige tidsbaserte data og funksjoner. Ved å kunne sammenligne to datoer, kan du enkelt sortere, filtrere og gjøre beregninger på dataene dine.

## Hvordan

TypeScript er et populært programmeringsspråk som kombinerer de nyttige funksjonene i JavaScript med statisk typetilordning. Å sammenligne datoer i TypeScript er enkelt og kan gjøres ved å bruke de innebygde Date-objektene.

For å sammenligne to datoer, må du først opprette et nytt Date-objekt for hver av datoene du ønsker å sammenligne. Deretter kan du bruke de forskjellige metoder og egenskaper til Date-objektet for å sammenligne dem. Her er et eksempel på hvordan du kan sammenligne to datoer for å se hvilken som kommer først:

```TypeScript
let dato1 = new Date("2021-01-01");
let dato2 = new Date("2021-01-05");

if (dato1 < dato2) {
  console.log("Dato1 kommer først.");
} else if (dato2 < dato1) {
  console.log("Dato2 kommer først.");
} else {
  console.log("Datoene er like.");
}

// output: Dato1 kommer først.
```

Her bruker vi de logiske operatorerne < og > for å sammenligne datoene. Du kan også bruke == og != for å sjekke om to datoer er like.

## Deep Dive

Når du sammenligner datoer, er det viktig å være klar over at de kan være på forskjellige formater og tidssoner. Dette kan føre til uventede resultater hvis du ikke tar hensyn til dette. Ved å bruke metoden getTime() på et Date-objekt, kan du få ut tiden i millisekunder siden 1. januar 1970. Dette gjør det enklere å sammenligne datoer uavhengig av format og tidssone.

Det er også verdt å merke seg at når du oppretter et Date-objekt, vil det automatisk bli satt til dagens dato og tid hvis ingen argumenter blir gitt. Derfor er det viktig å opprette Date-objekter med de riktige datoene og tidene du ønsker å sammenligne.

## Se også

- [TypeScript dokumentasjon om Date-objekter](https://www.typescriptlang.org/docs/handbook/dates-and-times.html)
- [W3Schools guide til dato og tid i JavaScript](https://www.w3schools.com/js/js_dates.asp)