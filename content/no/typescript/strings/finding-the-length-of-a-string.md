---
date: 2024-01-20 17:48:12.878062-07:00
description: "Hvordan gj\xF8re det: ."
lastmod: '2024-04-05T21:53:41.501460-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Finn lengden p\xE5 en streng"
weight: 7
---

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
