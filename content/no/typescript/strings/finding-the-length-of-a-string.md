---
title:                "Finn lengden på en streng"
aliases:
- /no/typescript/finding-the-length-of-a-string.md
date:                  2024-01-20T17:48:12.878062-07:00
model:                 gpt-4-1106-preview
simple_title:         "Finn lengden på en streng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å finne lengden på en streng betyr å telle hvor mange tegn den inneholder. Programmerere gjør dette for validering, begrensning av input, eller for å løkke gjennom hver bokstav.

## Hvordan gjøre det:
```TypeScript
let greeting: string = "Hei, verden!";
console.log(greeting.length); // Output: 12
```

```TypeScript
let emptyString: string = "";
console.log(emptyString.length); // Output: 0
```

```TypeScript
let emojiString: string = "👋🌍";
console.log(emojiString.length); // Output: 4 (emojier kan tas for 2 tegn hver)
```

## Dypdykk
Lengden på en streng i TypeScript hentes ganske enkelt ved å bruke `.length`-egenskapen, som arves fra JavaScript. Historisk sett, har denne egenskapen vært standarden siden de tidlige dagene av JavaScript. 

Viktige detaljer:
- `.length` returnerer antall 16-bits verdier i strengen, noe som kan være forvirrende med emojis eller andre multibyte tegn.
- Alternativer til `.length` for mer komplekse behov kan inkludere å bruke en String iterator eller en regex for riktig opptelling av tegn, spesielt når vi håndterer Unicode-tegn. 

## Se også:
- MDN Web Docs om `length`: [developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- TypeScript offisielle dokumentasjon: [www.typescriptlang.org/docs/](https://www.typescriptlang.org/docs/)
- Unicode og JavaScript-strings: [mathiasbynens.be/notes/javascript-unicode](https://mathiasbynens.be/notes/javascript-unicode)
