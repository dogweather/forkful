---
title:                "Sammenligning av to datoer"
html_title:           "Javascript: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Å sammenligne to datoer er en vanlig oppgave innenfor programmering. Dette innebærer å sjekke om en dato er før, etter eller lik en annen dato. Det er en viktig del av mange programmer og brukes blant annet for å sortere og filtrere data.

## Hvordan:
Du kan sammenligne to datoer ved å bruke innebygde funksjoner i Javascript. Du kan for eksempel bruke Date-objektet og sammenligne det med en annen dato. Her er et eksempel:

```Javascript
let date1 = new Date("2021-01-01");
let date2 = new Date("2021-01-15");

if (date1 < date2) {
  console.log("Dato 1 er før dato 2");
} else if (date1 > date2) {
  console.log("Dato 1 er etter dato 2");
} else {
  console.log("Dato 1 er lik dato 2");
}
```

Output: Dato 1 er før dato 2

## Dykk dypere:
Det å sammenligne datoer har vært en del av programmering helt siden tidlige datamaskiner. I dag er det mange forskjellige metoder for å sammenligne datoer, og noen programmeringsspråk har spesifikke funksjoner for dette. I tillegg til å bruke innebygde funksjoner i Javascript, kan du også bruke en tredjeparts bibliotek som Moment.js for å gjøre det enklere å håndtere og sammenligne datoer.

## Se også:
- [Date-objektet i Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js biblioteket](https://momentjs.com/)
- [Enkel guide til å sammenligne datoer i Javascript](https://www.digitalocean.com/community/tutorials/compare-dates-in-javascript)