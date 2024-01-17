---
title:                "Å skrive til standardfeil"
html_title:           "TypeScript: Å skrive til standardfeil"
simple_title:         "Å skrive til standardfeil"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

"## Hva & Hvorfor?"
Skriving til standard error er en måte for programmerere å kommunisere med brukeren på når noe uventet eller feil skjer i programmet. Dette gjøres ved å sende en feilmelding til standard error-strømmen i stedet for standard output-strømmen. Dette tillater brukeren å se nøyaktig hvor og hvorfor feilen oppsto.

"## Hvordan:"
```TypeScript
console.error("Feil oppsto ved linje 12: Ugyldig input"); 
// Output: Feil oppsto ved linje 12: Ugyldig input 
```
Ved å bruke console.error() i TypeScript, kan vi sende en spesifikk feilmelding til standard error-strømmen. Dette vil bli logget og synlig for brukeren, i tillegg til eventuelle andre handlinger som er satt opp for å håndtere feilen.

```TypeScript
let tall: number = "tre"; 
// Output: error TS2322: Type '"tre"' is not assignable to type 'number'.
```
I denne koden prøver vi å tildele en streng til en variabel som er definert som et tall. TypeScript vil da automatisk logge en feilmelding til standard error-strømmen og informere oss om hvilken linje og hvilken typefeil som har oppstått.

"## Dypdykk:"
Skriving til standard error har vært en vanlig praksis i programmering i mange år. Det tillater programvaren å fortsette å kjøre selv om feil oppstår, samtidig som den gir nødvendig informasjon til brukeren for å finne og rette opp feilen. Alternativet til å skrive til standard error er å bruke standard output, som i tillegg til å bli logget også kan bli brent til en fil for senere bruk.

Implementeringen av å skrive til standard error kan variere avhengig av programmeringsspråk, men i TypeScript kan dette gjøres ved hjelp av console.error() -funksjonen som nevnt ovenfor.

"## Se også:"
- ["Introduksjon til JavaScript feilhåndtering"](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Error_handling)
- ["The difference between console.log and console.error"](https://www.geeksforgeeks.org/difference-between-console-log-and-console-error-in-javascript/)
- ["Console object documentation in TypeScript"](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-3.html#object-spread-and-rest-for-javascript-types)