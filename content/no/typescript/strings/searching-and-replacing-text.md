---
date: 2024-01-20 17:58:50.456573-07:00
description: "Hvordan gj\xF8re det: S\xF8k og erstatt-funksjonaliteten har r\xF8tter\
  \ i tidlig tekstbehandling, som `sed` i Unix. I JavaScript og TypeScript bruker\
  \ vi\u2026"
lastmod: '2024-04-05T22:50:54.527331-06:00'
model: gpt-4-1106-preview
summary: "S\xF8k og erstatt-funksjonaliteten har r\xF8tter i tidlig tekstbehandling,\
  \ som `sed` i Unix."
title: "S\xF8king og erstatting av tekst"
weight: 10
---

## Hvordan gjøre det:
```TypeScript
const replaceText = (input: string, search: string, replaceWith: string): string => {
  return input.replace(new RegExp(search, 'g'), replaceWith);
};

// Eksempelbruk:
const originalText = 'Bananer er gule, bananer er sunne.';
const newText = replaceText(originalText, 'bananer', 'epler');

console.log(newText); // Epler er gule, epler er sunne.
```

## Dypdykk
Søk og erstatt-funksjonaliteten har røtter i tidlig tekstbehandling, som `sed` i Unix. I JavaScript og TypeScript bruker vi `String.prototype.replace`. Regex (regular expressions) brukes for å finne mønstre i teksten.

Alternativer til `replace` inkluderer tredjepartsbiblioteker som `lodash` for å håndtere mer avanserte erstatninger og manipulasjoner. Implementeringsdetaljer involverer ofte flagg, som 'g' for globalt søk, eller 'i' for case-insensitive matching.

## Se også
- MDN Web Docs om `String.prototype.replace`: https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- RegExp-guiden: https://developer.mozilla.org/docs/Web/JavaScript/Guide/Regular_Expressions
- Lodash bibliotek: https://lodash.com/docs/4.17.15#replace
