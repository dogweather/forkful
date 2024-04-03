---
date: 2024-01-20 17:39:29.234329-07:00
description: "How to (\"Slik gj\xF8r du det\") TypeScript gj\xF8r det lett \xE5 konvertere\
  \ tekst til sm\xE5 bokstaver med den innebygde metoden `.toLowerCase()`. Her er\
  \ et eksempel."
lastmod: '2024-03-13T22:44:40.519315-06:00'
model: gpt-4-1106-preview
summary: "TypeScript gj\xF8r det lett \xE5 konvertere tekst til sm\xE5 bokstaver med\
  \ den innebygde metoden `.toLowerCase()`."
title: "Konvertere en streng til sm\xE5 bokstaver"
weight: 4
---

## How to ("Slik gjør du det")
TypeScript gjør det lett å konvertere tekst til små bokstaver med den innebygde metoden `.toLowerCase()`. Her er et eksempel:

```TypeScript
let greeting: string = "Hei Verden!";
let lowerCaseGreeting: string = greeting.toLowerCase();

console.log(lowerCaseGreeting); // Output: "hei verden!"
```

Koden over viser hvordan strengen "Hei Verden!" blir endret til "hei verden!".

## Deep Dive ("Dypdykk")
Tilbake til JavaScripts spede begynnelse, har utviklerne hatt muligheten til å manipulere strenger. `.toLowerCase()` er en metode som har vært med lenge, arvet fra JavaScript og integrert i TypeScript for strengmanipulasjon. Alternativer finnes, som `.toLocaleLowerCase()`, som kan ta hensyn til lokalisering når den konverterer bokstaver. Imidlertid for de fleste engelskspråklige tilfeller, vil `.toLowerCase()` være tilstrekkelig.

Når det gjelder implementasjonen, bruker `.toLowerCase()` Unicode-verdier for å finne tilsvarende små bokstaver. Dette gjør det ikke bare funksjonelt for ASCII-tegn, men også for alfabetiske tegn i andre skripts som kyrillisk eller gresk.

## See Also ("Se Også")
- MDN Web Docs om toLowerCase(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase
- TypeScript Handbook: https://www.typescriptlang.org/docs/
- Unicode-standarden: https://www.unicode.org/standard/standard.html
