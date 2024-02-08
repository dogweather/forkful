---
title:                "Interpolering av en streng"
aliases:
- no/typescript/interpolating-a-string.md
date:                  2024-01-20T17:51:50.797922-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolering av en streng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Strenginterpolasjon er det å sette sammen en streng med variabler eller uttrykk. Programmerere bruker det for å lage dynamisk tekst og forbedre kodelesbarheten.

## Slik gjør du:
```TypeScript
let bruker = 'Ola';
let hilsen = `Hei, ${bruker}! Hvordan går det?`;
console.log(hilsen); // Output: Hei, Ola! Hvordan går det?
```

Et mer komplekst eksempel med uttrykk:
```TypeScript
let timer = 9;
let hilsen = `God ${timer < 12 ? 'morgen' : 'ettermiddag'}, verden!`;
console.log(hilsen); // Output: God morgen, verden!
```

## Dykk dypere
Før ES6 (ECMAScript 2015), måtte programmere klargjøre strenger med pluss-operatoren, som var rotete. Etter hvert som JavaScript-evolusjonen fortsatte, introduserte ES6 strengmaler (template literals), som tillater interpolasjon og flerlinjestrenger.

Alternativer til interpolasjon omfatter konkatenere strenger med `+` eller `.concat()` metoden, men disse metodene blir mindre brukt på grunn av deres verbositet.

Interpolering i TypeScript fungerer likt som i moderne JavaScript, siden TypeScript er et over-sett av JS. Interpolasjon evaluerer uttrykkene inne i `${...}` og konverterer resultatene til en streng verdi inne i den omliggende strengen.

## Se Også
- MDN Web Docs på Template literals: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals
- TypeScript Handbook om strenger: https://www.typescriptlang.org/docs/handbook/basic-types.html#string
