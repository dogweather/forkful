---
date: 2024-01-20 17:39:27.043380-07:00
description: "Hur g\xF6r man: Om vi blickar bak\xE5t var textbehandling med olika\
  \ skiftl\xE4gen v\xE4sentlig redan i tidiga databaser och textredigeringsprogram\
  \ f\xF6r att s\xE4kerst\xE4lla\u2026"
lastmod: '2024-04-05T21:53:38.972354-06:00'
model: gpt-4-1106-preview
summary: "Om vi blickar bak\xE5t var textbehandling med olika skiftl\xE4gen v\xE4\
  sentlig redan i tidiga databaser och textredigeringsprogram f\xF6r att s\xE4kerst\xE4\
  lla konsekvent datahantering."
title: "Konvertera en str\xE4ng till gemener"
weight: 4
---

## Hur gör man:
```TypeScript
let greeting: string = "Hej Världen!";
let lowerCaseGreeting: string = greeting.toLowerCase();
console.log(lowerCaseGreeting); // "hej världen!"
```

## Djupdykning:
Om vi blickar bakåt var textbehandling med olika skiftlägen väsentlig redan i tidiga databaser och textredigeringsprogram för att säkerställa konsekvent datahantering. I TypeScript är `.toLowerCase()` metoden inbyggd i `String` objektet och mycket rättfram. Förutom `.toLowerCase()` finns alternativ som `.toLocaleLowerCase()` för språkspecifik hantering där vissa kulturer definierar ytterligare regler för omvandling till små bokstäver. Sättet dessa metoder implementeras på kan skilja sig mellan olika JavaScript-motorer; de flesta bygger dock på Unicode-standarder för att säkerställa bred kompatibilitet.

## Se också:
- [MDN Web Docs - toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [ECMAScript Language Specification](https://www.ecma-international.org/ecma-262/10.0/index.html#sec-string.prototype.tolowercase)
- [Unicode Character Database](https://www.unicode.org/ucd/)
