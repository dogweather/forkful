---
title:                "Utdrag av understrenger"
html_title:           "Bash: Utdrag av understrenger"
simple_title:         "Utdrag av understrenger"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Uttrekking av substrings er prosessen med å isolere spesifikke deler av en streng. Programmerere gjør det ofte for å manipulere, sammenligne eller analysere data.

## Hvordan:

Her er et enkelt eksempel på bruk av `substr()`-funksjonen i TypeScript.

```typescript
let tekst: string = "Hei, Verden!";
let delTekst: string = tekst.substr(0, 3);
console.log(delTekst);
```

Denne koden vil skrive ut følgende:

```typescript
"Hei"
```
Det tar to argumenter: den første er startindeksen, og den andre er antallet tegn du vil utvinne.

## Dypdykk

Uttrekking av substrings har vært en grunnleggende operasjon av mange programmeringsspråk, inkludert tidligere versjoner av JavaScript. I TypeScript, anbefaler vi bruk av `slice()` over `substr()` som det er mer forutsigbart.

`substr()` kan gi uventet oppførsel med negative indekser, mens `slice()` håndterer dem mer konsekvent. Her er et eksempel:

```typescript
let tekst: string = "Hei, Verden!";
console.log(tekst.slice(-1));   // utskrifter: "!",
console.log(tekst.substr(-1));  // utskrifter: "Verden!"
```

## Se Også 

- MDN Web Docs for detaljer om [`substr()`](https://developer.mozilla.org/nb/docs/Web/JavaScript/Reference/Global_Objects/String/substr) og [`slice()`](https://developer.mozilla.org/nb/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- Microsofts TypeScript Handbook for mer informasjon om [strenger i TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#string).