---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:58.533843-07:00
description: "Een string omzetten naar kleine letters betekent dat elk teken in de\
  \ string een kleine letter wordt. Programmeurs doen dit voor consistentie, vooral\
  \ voor\u2026"
lastmod: '2024-03-13T22:44:50.536418-06:00'
model: gpt-4-0125-preview
summary: Een string omzetten naar kleine letters betekent dat elk teken in de string
  een kleine letter wordt.
title: Een string omzetten naar kleine letters
weight: 4
---

## Hoe:
In TypeScript is het omzetten van een string naar kleine letters appeltje-eitje. Roep gewoon `.toLowerCase()` aan op je string. Zo doe je dat:

```typescript
let myString: string = "HeLLo, WorLD!";
let lowerCaseString: string = myString.toLowerCase();
console.log(lowerCaseString); // Uitvoer: "hello, world!"
```

Makkelijk, hè?

## Diepere Duik
Vroeger was tekstverwerking niet altijd consequent, en kon karaktercodering een wilde westen zijn. Nu, met Unicode en gestandaardiseerde methoden, zijn cases uniform over talen. In vergelijking met `.toLowerCase()`, is een oude school benadering (zoals ASCII-manipulatie) steentijd. Alternatieven (zoals `.toLocaleLowerCase()`) houden rekening met lokaal-specifieke regels voor juiste casing, wat handig kan zijn. Onder de motorkap gaat `.toLowerCase()` in JavaScript (en bij uitbreiding TypeScript) door elk teken en, als het een hoofdletter is, transformeert het naar zijn kleine-letter-equivalent op basis van Unicode-mapping.

## Zie Ook
Voor meer string gymnastiek en om je tekstverwerkingsvaardigheden op te krikken, neem hier een kijkje:

- MDN Documentatie over `.toLowerCase()`: [MDN toLowerCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- Officiële TypeScript Documentatie: [TypeScriptlang.org](https://www.typescriptlang.org/docs/)
- Om lokatie-specifieke transformaties beter te begrijpen: [MDN toLocaleLowerCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)
- Voor diepgaande Unicode standaarden: [Unicode Case Mapping](https://www.unicode.org/reports/tr21/tr21-5.html)
