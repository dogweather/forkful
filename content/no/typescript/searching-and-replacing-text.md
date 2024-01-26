---
title:                "Søking og erstatting av tekst"
date:                  2024-01-20T17:58:50.456573-07:00
model:                 gpt-4-1106-preview
simple_title:         "Søking og erstatting av tekst"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Søk og erstatt i tekstbehandling lar deg finne strenger og bytte dem ut med noe annet. Vi programmerere gjør det for å effektivisere kodeendringer, rette feil, eller oppdatere data.

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
