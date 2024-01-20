---
title:                "Interpolering av en streng"
html_title:           "Bash: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Strenginterpolasjon er en metode for å injisere variable verdier inn i en tekststreng. Programmerere bruker denne teknikken for å gjøre koden mer lesbar og vedlikeholdbar.

## Hvordan:

Her er et eksempel på hvordan du bruker strenginterpolasjon i TypeScript. 

```TypeScript
let navn = 'Ola';
console.log(`Hei, ${navn}!`);
```

Output vil være:

``` 
Hei, Ola!
```

## Dyp Dykk

Strenginterpolasjon, også kjent som streng substitusjon, har blitt brukt innen programmering i mange år, og det kan bli funnet i mange språk ettersom det gjør koden klar og forståelig. 

I TypeScript, som er en supersett av JavaScript, er det en mulighet å bruke bak-tick (`) for å definere en streng og ${} for å interpolere variabler.

Et alternativ til strenginterpolasjon kan være '+' eller konkatenasjon operator:

```TypeScript
let navn = 'Ola';
console.log('Hei, ' + navn + '!');
```

Selv om det fungerer, så kan det bli forvirrende hvis det er mange variable eller hvis tekststrengen er lang.

## Se Også

For mer informasjon om strenginterpolasjon og andre relaterte konsepter, sjekk ut disse linkene:

1. [Mozilla Developer Network (MDN) - Template literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)

2. [TypeScript Official Documentation - String Interpolation](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html)