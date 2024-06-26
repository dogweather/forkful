---
date: 2024-01-20 17:35:49.184447-07:00
description: "How to: I gamle dager brukte man ofte plussoperatoren (`+`) til \xE5\
  \ sette sammen strenger. Det fungerte greit, men kunne bli rotete med mange variabler\
  \ og\u2026"
lastmod: '2024-04-05T22:50:54.533674-06:00'
model: gpt-4-1106-preview
summary: "I gamle dager brukte man ofte plussoperatoren (`+`) til \xE5 sette sammen\
  \ strenger."
title: "Sammensl\xE5ing av strenger"
weight: 3
---

## How to:
```TypeScript
let hello: string = "Hei";
let world: string = "verden";
let greeting: string = hello + " " + world + "!";
console.log(greeting); // Output: Hei verden!
```

Eller med template literals:

```TypeScript
let hello: string = "Hei";
let world: string = "verden";
let greeting: string = `${hello} ${world}!`;
console.log(greeting); // Output: Hei verden!
```

## Deep Dive
I gamle dager brukte man ofte plussoperatoren (`+`) til å sette sammen strenger. Det fungerte greit, men kunne bli rotete med mange variabler og tekststykker.

ES6 introduserte "template literals" som lar deg lage mer lesbare strenger med variabler direkte inne i teksten, angitt med `${}`. I tillegg til lesbarhet, gjør dette det enklere å inkludere uttrykk og multiline strings.

Det er også andre metoder som `concat()` funksjonen eller `join()` på arrays, men disse er mer aktuelle når man jobber med lister av strenger.

TypeScript, som er en overbygning over JavaScript, følger disse samme metodene for strengsammensetning og legger til type-sjekking for ekstra sikkerhet.

## See Also
- MDN Web Docs on String concatenation: [String concatenation - JavaScript | MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- Stack Overflow discussion on best practices: [When to use template strings in TypeScript?](https://stackoverflow.com/questions/37371364/when-to-use-template-strings-in-typescript)
