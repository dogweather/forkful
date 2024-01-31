---
title:                "Een string met hoofdletters maken"
date:                  2024-01-28T21:55:33.244840-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string met hoofdletters maken"

category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/typescript/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een string kapitaliseren betekent het transformeren van de eerste letter van elk woord naar een hoofdletter en de rest naar kleine letters. Programmeurs doen dit voor consistentie in opmaak over gebruikersinterfaces heen en om ervoor te zorgen dat eigennamen en titels correct worden weergegeven.

## Hoe:

Hier is wat snelle TypeScript om je strings te kapitaliseren:

```typescript
function capitalizeString(input: string): string {
  return input.replace(/\w\S*/g, (woord) => {
    return woord.charAt(0).toUpperCase() + woord.substr(1).toLowerCase();
  });
}

// Voorbeeldgebruik:
const titel = "hello world from TypeScript";
const gekapitaliseerdeTitel = capitalizeString(titel);
console.log(gekapitaliseerdeTitel); // Uitvoer: "Hello World From Typescript"
```

Makkelijk, toch? Nu ga je die kleine strings omtoveren tot iets chiques!

## Diepe Duik

Kapitalisatie bestaat al sinds de tijd van oude schriften, ter verfijning van de leesbaarheid. In programmering, buiten esthetische en grammaticale correctheid, kan het kapitaliseren van strings cruciaal zijn voor vergelijkingsoperaties waar "Apple" en "apple" anders behandeld kunnen worden.

Alternatieven voor de `capitalizeString` functie kunnen bibliotheken zoals Lodash omvatten, die de `_.startCase` methode bieden, of steunen op CSS voor visuele kapitalisatie (`text-transform: capitalize;`). Echter, CSS verandert de daadwerkelijke waarde van de string niet, alleen de weergave.

JavaScript had oorspronkelijk geen ingebouwde methode voor stringkapitalisatie, wat het aan de creativiteit van ontwikkelaars overliet. De functie hierboven gebruikt een reguliere expressie om woordgrenzen `\w\S*` te identificeren, kapitaliseert de eerste letter met `toUpperCase()`, en de rest met `toLowerCase()`.

## Zie Ook

- MDN String Documentatie: [https://developer.mozilla.org/nl/docs/Web/JavaScript/Reference/Global_Objects/String](https://developer.mozilla.org/nl/docs/Web/JavaScript/Reference/Global_Objects/String)
- Lodash's `_.startCase` functie: [https://lodash.com/docs/#startCase](https://lodash.com/docs/#startCase)
- String.prototype.toLocaleUpperCase (voor locatiegevoelige transformaties): [https://developer.mozilla.org/nl/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleUpperCase](https://developer.mozilla.org/nl/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleUpperCase)
