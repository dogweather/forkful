---
aliases:
- /sv/typescript/converting-a-string-to-lower-case/
date: 2024-01-20 17:39:27.043380-07:00
description: "Att konvertera en str\xE4ng till gemener inneb\xE4r att omvandla alla\
  \ stora bokst\xE4ver till sm\xE5 i en given text. Programmerare g\xF6r det f\xF6\
  r att standardisera\u2026"
lastmod: 2024-02-18 23:08:51.531190
model: gpt-4-1106-preview
summary: "Att konvertera en str\xE4ng till gemener inneb\xE4r att omvandla alla stora\
  \ bokst\xE4ver till sm\xE5 i en given text. Programmerare g\xF6r det f\xF6r att\
  \ standardisera\u2026"
title: "Konvertera en str\xE4ng till gemener"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera en sträng till gemener innebär att omvandla alla stora bokstäver till små i en given text. Programmerare gör det för att standardisera data, förbättra sökbarheten eller möjliggöra jämförelser oberoende av skiftläge.

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
