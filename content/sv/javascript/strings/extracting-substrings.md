---
title:                "Extrahera delsträngar"
aliases:
- /sv/javascript/extracting-substrings.md
date:                  2024-01-20T17:46:01.934098-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extrahera delsträngar"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att extrahera delsträngar i JavaScript handlar om att plocka ut specifika delar ur en sträng. Programmerare gör detta för att manipulera och använda data mer effektivt, som att hämta användarnamn från email-adresser eller visa delar av text på en användargränssnitt.

## Hur man gör:

Extrahera en delsträng med `substring()`, `slice()` eller `substr()` (fast den sistnämnda är föråldrad).

```javascript
let text = "Hej, jag heter Sven!";
let delstrang = text.substring(4, 7); // "jag"
console.log(delstrang);

delstrang = text.slice(-5, -1); // "Sven"
console.log(delstrang);

// Notera: substr() är föråldrad men så här skulle det se ut:
delstrang = text.substr(11, 4); // "heter"
console.log(delstrang);
```

## Djupdykning

Förr användes ofta `substr()`, men den är inte rekommenderad längre då den är föråldrad. `substring()` och `slice()` är de rätta valen nu. De skiljer sig åt när det gäller att hantera negativa index: `slice()` kan hantera dessa medan `substring()` omvandlar negativa index till 0. Det är viktigt att välja rätt metoder för rätt syfte, speciellt när man hanterar större och mer komplexa strängar.

## Se även:

- MDN Web Docs för `substring()`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring
- MDN Web Docs för `slice()`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice
- En diskussion om `substr()`s avskrivning på TC39 GitHub: https://github.com/tc39/proposal_string_replaceAll/issues/34
