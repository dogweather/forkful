---
date: 2024-01-20 17:47:35.772309-07:00
description: "I JavaScript bestemmer vi lengden p\xE5 en streng med `.length`-egenskapen.\
  \ Det hjelper oss med \xE5 h\xE5ndtere tekst, som \xE5 validering av input eller\
  \ looping\u2026"
lastmod: '2024-03-13T22:44:41.175252-06:00'
model: gpt-4-1106-preview
summary: "I JavaScript bestemmer vi lengden p\xE5 en streng med `.length`-egenskapen.\
  \ Det hjelper oss med \xE5 h\xE5ndtere tekst, som \xE5 validering av input eller\
  \ looping\u2026"
title: "Finn lengden p\xE5 en streng"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
I JavaScript bestemmer vi lengden på en streng med `.length`-egenskapen. Det hjelper oss med å håndtere tekst, som å validering av input eller looping gjennom hver karakter.

## Hvordan:
```javascript
let greeting = "Hei, verden!";
console.log(greeting.length); // Output: 13

let emptyString = "";
console.log(emptyString.length); // Output: 0

let multilineString = `Første linje
Andre linje`;
console.log(multilineString.length); // Output: 24 (inkludert newline-tegn)
```

## Dypdykk
Historisk sett har `.length` vært den enkleste og mest rett fram metoden for å få lengden av en streng i JavaScript. Det teller antall enheter i strengen, ikke nødvendigvis antall tegn. For moderne brukere er dette viktig siden emojis eller enkelte språktegn kan bruke flere enheter.

Alternativt kan du bruke en løkke for å telle karakterene manuelt, men dette er ikke effektivt. JavaScript `for...of` kan brukes til å få riktig antall tegn, selv for de som tar mer enn én enhet:

```javascript
let specialString = "👋🌍";
let charCount = 0;
for (let char of specialString) {
  charCount++;
}
console.log(charCount); // Output: 2
```

Implementasjonsdetaljer: JavaScript bruker UTF-16-koding for strenger. `.length` returnerer antall 16-bits verdier, noe som kan være forvirrende med tegn kodet som 2 UTF-16 enheter.

## Se Også
- MDN Web Docs om `.length`: [MDN - String.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- En diskusjon på Stack Overflow om strenglengde og spesialtegn: [Stack Overflow - Javascript string length](https://stackoverflow.com/questions/543695/javascript-string-length-and-special-characters)
