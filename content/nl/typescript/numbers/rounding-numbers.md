---
title:                "Afronden van getallen"
aliases:
- /nl/typescript/rounding-numbers/
date:                  2024-01-28T22:07:09.373717-07:00
model:                 gpt-4-0125-preview
simple_title:         "Afronden van getallen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/typescript/rounding-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Rondgetallen is het bijsnijden van een getal naar een specifieke precisie. Programmeurs doen dit om de numerieke uitvoer te beheersen voor leesbaarheid, weergavedoeleinden of wanneer specifieke precisie vereist is na operaties die resultaten met zwevende komma opleveren.

## Hoe:
Afronden in TypeScript kan worden gedaan met behulp van verschillende methoden. Hier is een snelle doorloop:

```typescript
// Math.round rondt af naar het dichtstbijzijnde gehele getal
console.log(Math.round(1.5)); // Uitvoer: 2

// Math.ceil rondt naar boven af naar het dichtstbijzijnde gehele getal
console.log(Math.ceil(1.1)); // Uitvoer: 2

// Math.floor rondt naar beneden af naar het dichtstbijzijnde gehele getal
console.log(Math.floor(1.8)); // Uitvoer: 1

// toFixed rondt af naar een vast aantal decimalen
let num = 1.23456;
console.log(num.toFixed(2)); // Uitvoer: "1.23"
// Opmerking: toFixed retourneert een string! Gebruik parseFloat om indien nodig terug te converteren.
console.log(parseFloat(num.toFixed(2))); // Uitvoer: 1.23
```

## Verdieping
Vroeger was afronden een must vanwege beperkte ruimte en precisieproblemen in vroege computers. Tegenwoordig kan rekenen met zwevende komma leiden tot eigenaardige resultaten vanwege hoe getallen in binaire vorm worden opgeslagen. Alternatieven voor afronden zijn floor, ceil en trunc (voor het afsnijden van decimalen zonder af te ronden).

Het is de moeite waard om de interne werking te noteren: `Math.round` volgt de "rond half omhoog" (ook bekend als "commercieel afronden"), terwijl `Math.floor` en `Math.ceil` eenvoudig zijn. `toFixed` kan onverwachte resultaten veroorzaken omdat het een string retourneert, en het rondt af met "rond half naar even" (ook bekend als "bankiers afronding"), vooral nuttig om bias bij het meerdere keren afronden van dezelfde getallen te verminderen.

## Zie Ook
- [MDN - Math.round()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/round)
- [MDN - Math.ceil()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/ceil)
- [MDN - Math.floor()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/floor)
- [MDN - toFixed()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
- [IEEE Standaard voor Zwevend-kommagetallenrekenen (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
