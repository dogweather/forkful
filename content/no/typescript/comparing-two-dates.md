---
title:                "Sammenligner to datoer"
html_title:           "Clojure: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Sammenligning av to datoer i TypeScript

## Hva og hvorfor?
Sammenligning av to datoer betyr å bestemme hvilken dato som er tidligere, senere, eller om begge datoene er like. Dette er nyttig for programmerere å bestemme hendelser og evaluerer forhold basert på tid.

## Hvordan?

Sammenlign to datoer i TypeScript er enkelt. Her er noen eksempler:

```TypeScript
let dato1 = new Date(2020, 11, 31);
let dato2 = new Date(2021, 0, 1);

// Sjekke om dato1 er etter dato2
console.log(dato1 > dato2); // false 

// Sjekke om dato1 er før dato2
console.log(dato1 < dato2); // true

// Sjekke om dato1 er lik dato2
console.log(dato1.getTime() === dato2.getTime()); // false 
```

## Dypdykk

1. **Historisk kontekst**: JavaScript, og dermed TypeScript, har hatt evnen til å sammenligne datoer siden begynnelsen. Men, implementeringen har endret seg over tid for å bli mer presis og pålitelig.
2. **Alternativer**: Du kan bruke biblioteker som Moment.js for mer robuste dato- og tidshåndteringsfunksjoner.
3. **Implementeringsdetaljer**: Når du sammenligner datoer i TypeScript (eller JavaScript), sammenlignes millisekundene siden 1. Januar 1970 (Unix Epoch). Derfor er det mulig å sammenligne datoer med matematiske operatorer.

## Se også 

For mer informasjon om datoer i JavaScript og TypeScript, sjekk ut disse lenkene:

- [Mozilla Developer Network (MDN) Date Reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js Library](https://momentjs.com/)
- [Understanding Dates in TypeScript/JavaScript](https://javascript.info/date)