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

## Hvorfor

Mange ganger i programmering må vi sammenligne to datoer for å avgjøre om en dato kommer før, etter eller på samme tid som en annen. Dette kan være nyttig når man for eksempel ønsker å filtrere data eller sortere det i kronologisk rekkefølge. Med TypeScript kan vi enkelt sammenligne datoer ved hjelp av innebygde funksjoner og metoder.

## Hvordan

For å sammenligne to datoer i TypeScript, kan vi bruke den innebygde Date-klassen. Vi kan opprette to Date-objekter og deretter bruke metoden `.getTime()` for å få en timestamp som vi kan sammenligne. La oss se på et eksempel:

```TypeScript
const birthday = new Date(2000, 5, 10);
const currentDate = new Date();

if (birthday.getTime() > currentDate.getTime()) {
  console.log("Birthday has not happened yet");
} else if (birthday.getTime() < currentDate.getTime()) {
  console.log("Birthday has already passed");
} else {
  console.log("Today is the birthday!");
}
```

I dette eksempelet oppretter vi et Date-objekt for en fødselsdag i 2000 (10. juni) og et Date-objekt for dagens dato. Vi sammenligner deretter timestampene til disse to datoene ved hjelp av `.getTime()`-metoden. Hvis fødselsdagen ligger etter dagens dato, betyr det at den ikke har skjedd ennå. Hvis fødselsdagen ligger før dagens dato, har den allerede passert. Hvis timestampene er like, betyr det at det er fødselsdagen i dag.

## Deep Dive

I tillegg til `.getTime()`-metoden, kan vi også bruke andre innebygde metoder og operatører for å sammenligne datoer i TypeScript. Noen nyttige er `.getFullYear()`, `.getMonth()` og `.getDate()` for å få spesifikke deler av en dato. I tillegg kan vi bruke operatorer som `>`, `<` og `===` for å sammenligne datoene direkte, uten å bruke `.getTime()`-metoden.

Det er også viktig å merke seg at datoer kan være litt feilbarlige på grunn av forskjellige tids soner og formater. Det kan være lurt å utforske forskjellige biblioteker og pakker som kan hjelpe med å normalisere datoer og håndtere ulike tids soner.

## Se også

- [TypeScript dokumentasjon](https://www.typescriptlang.org/docs/)
- [Date-klassen i TypeScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js - library for working with dates in JavaScript](https://momentjs.com/)