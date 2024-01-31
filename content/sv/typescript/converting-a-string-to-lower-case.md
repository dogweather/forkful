---
title:                "Konvertera en sträng till gemener"
date:                  2024-01-20T17:39:27.043380-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertera en sträng till gemener"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/converting-a-string-to-lower-case.md"
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
