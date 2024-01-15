---
title:                "Konvertere en dato til en streng"
html_title:           "TypeScript: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Å konvertere en dato til en streng er et vanlig oppgave når man jobber med programmering. Dette kan være nyttig når man for eksempel trenger å vise en dato i et grafisk grensesnitt eller lagre den i en database.

## Hvordan
For å konvertere en dato til en streng i TypeScript, kan man bruke den innebygde metoden `toString()`. Denne metoden tar imot et valgfritt parameter som spesifiserer formatet på datoen. Her er et eksempel på hvordan man kan bruke denne metoden:

```TypeScript
let today = new Date();
let stringDate = today.toString('MM/dd/yyyy');
console.log(stringDate);
```

Output vil være: `09/15/2021`. Her har vi spesifisert at datoen skal være i måned/dag/år-formatet.

Man kan også bruke andre formateringsalternativer som `toDateString()` for å bare få datoen uten klokkeslettet eller `toLocaleDateString()` for å få en lokal datetime string basert på brukerens sted.

## Dypdykk
Når man bruker `toString()` eller lignende metoder, vil man merke at datoen kan bli formatert på forskjellige måter avhengig av nettleseren eller operativsystemet man bruker. Dette kan føre til uforutsette problemer, spesielt når man jobber med internasjonale brukere. For å unngå dette, kan man bruke biblioteker som Moment.js som har mer robuste funksjoner for håndtering av datoer og formatering av disse.

## Se også
- [Offisiell dokumentasjon for Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js biblioteket](https://momentjs.com/)