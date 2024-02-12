---
title:                "Interpolering av en streng"
aliases:
- /no/javascript/interpolating-a-string.md
date:                  2024-01-20T17:50:59.943159-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolering av en streng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Interpolering av strenger i JavaScript betyr å sette sammen bitene av tekst og variabler til en helhetlig streng. Vi gjør det for å lage dynamiske meldinger, bygge spørringer, og for at koden skal være mer lesbart og vedlikeholde.

## Hvordan gjøre det:
```javascript
// Eksempel med Template Literals (ES6+)
const navn = 'Ola';
const hilsen = `Hei, ${navn}! Hvordan har du det?`;
console.log(hilsen);  // Output: Hei, Ola! Hvordan har du det?

// Gammel metode med konkatenasjon
const gammelHilsen = 'Hei, ' + navn + '! Hvordan har du det?';
console.log(gammelHilsen);  // Output: Hei, Ola! Hvordan har du det?
```

## Dypdykk
I gamle JavaScript brukte vi '+' for å sette sammen strenger og variabler. Etter ES6 (ECMAScript 2015) innførte vi template literals, som gjør det raskere og ryddigere. I stedet for å bruke plusstegn, omslutter du strengen med backticks (\`) og plasserer variabler eller uttrykk inni `${}`. Dette er mer fleksibelt siden du også kan sette inn uttrykk, kalle funksjoner og til og med ha flerlinjetekst uten ekstra arbeid. Det finnes alternativer som `sprintf` i andre språk, men i JavaScript holder template literals stort sett mål.

## Se også
- MDN Web Docs om template literals: [MDN Template Literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- Du kan også utforske biblioteker som `lodash` for ekstra string-manipuleringsfunksjoner: [Lodash](https://lodash.com/docs/#template)
