---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:49.371745-07:00
description: "Hur man g\xF6r: TypeScript, som \xE4r en ut\xF6kning av JavaScript,\
  \ till\xE5ter olika metoder f\xF6r att g\xF6ra f\xF6rsta bokstaven i en str\xE4\
  ng stor, allt fr\xE5n rena\u2026"
lastmod: '2024-03-13T22:44:37.639583-06:00'
model: gpt-4-0125-preview
summary: "TypeScript, som \xE4r en ut\xF6kning av JavaScript, till\xE5ter olika metoder\
  \ f\xF6r att g\xF6ra f\xF6rsta bokstaven i en str\xE4ng stor, allt fr\xE5n rena\
  \ JavaScript-ansatser till att anv\xE4nda tredjepartsbibliotek f\xF6r mer komplexa\
  \ eller specifika anv\xE4ndningsfall."
title: "G\xF6r om en str\xE4ng till versaler"
weight: 2
---

## Hur man gör:
TypeScript, som är en utökning av JavaScript, tillåter olika metoder för att göra första bokstaven i en sträng stor, allt från rena JavaScript-ansatser till att använda tredjepartsbibliotek för mer komplexa eller specifika användningsfall.

**Ren JavaScript-ansats:**

```typescript
function capitalize(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

// Exempelutdata:
console.log(capitalize('hello TypeScript!')); // 'Hello TypeScript!'
```

Denna metod är okomplicerad och förlitar sig på `charAt()`-metoden för att komma åt den första bokstaven i strängen och `toUpperCase()` för att konvertera den till versal. Metoden `slice(1)` hämtar sedan resten av strängen, lämnar den oförändrad.

**Använda Lodash-biblioteket:**

För projekt som redan använder [Lodash](https://lodash.com/)-biblioteket kan du använda dess `_.capitalize`-funktion för att uppnå samma resultat med mindre mallkod.

Installera först Lodash:

```bash
npm install lodash
```

Använd sedan det i din TypeScript-fil:

```typescript
import * as _ from 'lodash';

// Exempelutdata:
console.log(_.capitalize('hello TypeScript!')); // 'Hello typescript!'
```

Obs: Lodashs `_.capitalize`-metod gör resten av strängen till gemener vilket inte alltid kan vara vad du vill.

**Använda ett reguljärt uttryck:**

Ett reguljärt uttryck kan erbjuda ett koncist sätt att göra den första bokstaven i en sträng stor, särskilt om du behöver göra den första bokstaven i varje ord i en sträng stor.

```typescript
function capitalizeWords(str: string): string {
  return str.replace(/\b\w/g, char => char.toUpperCase());
}

// Exempelutdata:
console.log(capitalizeWords('hello typescript world!')); // 'Hello Typescript World!'
```

Denna metod använder `replace()`-funktionen för att söka efter varje ordgräns följt av en alfanumerisk karaktär (`\b\w`), och gör varje träff stor. Den är särskilt praktisk för titlar eller rubriker.
