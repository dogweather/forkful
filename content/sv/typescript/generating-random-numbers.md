---
title:                "Generera slumpmässiga tal"
date:                  2024-01-20T17:49:56.587013-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generera slumpmässiga tal"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumptal betyder att skapa nummer som inte förutsägs eller följer något mönster. Programmerare använder slumptal för spel, simuleringar, säkerhetstester och där det behövs element av chans.

## How to:

```typescript
function getRandomNumber(min: number, max: number): number {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Använd funktionen för att skapa ett slumptal mellan 1 och 10
console.log(getRandomNumber(1, 10));
```
Sample output:
```
5
```

## Deep Dive
Historiskt sett, innan datorernas tid, använde människor tärningar, lottpåsar eller andra fysiska metoder för att skapa chans och slump. I datorvärlden simuleras detta med pseudoslumptalsgeneratorer (PRNGs), som är deterministiska men försöker efterlikna slumpmässighet.

Ett alternativ är att använda krypto-säkra generatorer (`crypto.getRandomValues()` i webbläsaren), som är viktiga för säkerhetskrävande uppgifter.

Implementationen i TypeScript använder `Math.random()`, som är bra för de flesta ändamål men inte rekommenderas för kryptografiskt starka behov eftersom det inte är tillräckligt oförutsägbart.

## See Also

- Mozilla Developer Network om `Math.random()`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random
- Web Crypto API: https://developer.mozilla.org/en-US/docs/Web/API/Web_Crypto_API
- Ett djupdyk i hur pseudoslumptal genereras: https://www.random.org/randomness/
