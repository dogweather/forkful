---
title:                "Store bokstaver i en streng"
html_title:           "TypeScript: Store bokstaver i en streng"
simple_title:         "Store bokstaver i en streng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å kapitalisere en streng er å gjøre den første bokstaven stor. Dette gjøres ofte for å gjøre tekst mer leselig og ryddig. I programmering, spesielt i strings og variabelnavn, er det en vanlig praksis for å følge god kodekonvensjon.

## Slik gjør du:
```TypeScript
// Eksempel 1:
const navn = "per hansen";
console.log(navn.charAt(0).toUpperCase() + navn.slice(1));
// Output: "Per hansen";

// Eksempel 2:
const fruit = "eple";
console.log(fruit[0].toUpperCase() + fruit.slice(1));
// Output: "Eple";
```

## Dypdykk:
Kapitalisering av strings ble først brukt i maskinlesing og maskinoversetting på tidlig 1950-tallet. I dag brukes det i de fleste programmeringsspråk, inkludert TypeScript. Det finnes også ulike metoder for å kapitalisere en streng, som for eksempel å bruke CSS for å style første bokstav.

## Se også:
- [String.prototype.charAt()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [String.prototype.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)