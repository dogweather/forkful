---
date: 2024-01-20 17:46:51.983544-07:00
description: "How to: Extrahera en delstr\xE4ng i TypeScript? Anv\xE4nd `substring`,\
  \ `slice` eller `substr`. S\xE5 h\xE4r."
lastmod: '2024-03-13T22:44:37.645294-06:00'
model: gpt-4-1106-preview
summary: "Extrahera en delstr\xE4ng i TypeScript."
title: "Extrahera delstr\xE4ngar"
weight: 6
---

## How to:
Extrahera en delsträng i TypeScript? Använd `substring`, `slice` eller `substr`. Så här:

```TypeScript
let fullString: string = "Hej världen!";

// Använd substring
let delString: string = fullString.substring(0, 3); // "Hej"

// Använd slice
let sliceString: string = fullString.slice(4, 11); // "världen"

// Använd substr (även om det är på väg bort!)
let substrString: string = fullString.substr(4, 7); // "världen"

console.log(delString); // Output: "Hej"
console.log(sliceString); // Output: "världen"
console.log(substrString); // Output: "världen"
```

## Deep Dive
Extraktion av delsträngar har länge varit en viktig del av programmering. Historiskt sett har olika språk erbjudit olika funktioner, som `substring`, `slice`, och `substr`. I TypeScript (och i JavaScript det bygger på) är `substring` och `slice` de vanligaste och rekommenderas, eftersom de är mer förutsägbara och `substr` är föråldrat. Men vad är skillnaderna?

- `substring(indexStart: number, indexEnd?: number): string`
  Tar en startindex och en slutindex. Om slutindex är uteblivet, sträcker sig extraktionen till slutet av strängen.

- `slice(start?: number, end?: number): string`
  Fungerar likadant som `substring` men kan ta negativa index som räknas bakifrån och är mer flexibel.

- `substr(start: number, length?: number): string`
  Tar en startposition och antal tecken att extrahera. På väg ut, så använd detta sparande.

Val av metod beror på situationen, men `slice` ger större flexibilitet och följer moderna webbstandarder.

## See Also
- [String.prototype.substring() - MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [String.prototype.slice() - MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [String.prototype.substr() - MDN (Obsolete)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
