---
title:    "TypeScript: Å finne lengden av en streng"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor bry seg med å finne lengden på en streng? Det kan virke som en enkel oppgave, men det er faktisk veldig nyttig i mange programmeringssituasjoner. Ved å kunne finne lengden på en streng, kan du enkelt manipulere den og hente ut bestemte deler av den.

## Hvordan
```TypeScript
const streng = "Hei, dette er en streng!"
console.log(streng.length);
```
Dette vil resultere i `24` i konsollen, siden det er 24 tegn i strengen. Du kan også bruke `.length`-metoden på en variabel som allerede er definert som en streng.

```TypeScript
let navn: string = "Sofie";
console.log(navn.length);
```
Dette vil resultere i `5` i konsollen, siden "Sofie" består av 5 tegn.

## Dypdykk
I TypeScript er `.length` en innebygd egenskap for strenger. Dette betyr at vi kan bruke den uten å måtte importere eller definere den. Sammenlignet med andre programmeringsspråk, hvor det kan være nødvendig å bruke en spesifikk metode for å finne lengden på en streng, er dette en enklere og mer intuitiv metode.

En viktig ting å merke seg er at `.length` returnerer antall tegn i strengen, inkludert mellomrom og spesialtegn. Dette kan være nyttig å vite hvis du jobber med manipulasjon av strenger. I tillegg kan `.length` også brukes på arrays for å finne antall elementer i det.

## Se Også
- MDN: String.prototype.length
(https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- TypeScript Handbook: Strings
(https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- W3Schools: TypeScript String Length
(https://www.w3schools.com/jsref/jsref_length_string.asp)