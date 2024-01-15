---
title:                "Å konvertere en streng til små bokstaver"
html_title:           "TypeScript: Å konvertere en streng til små bokstaver"
simple_title:         "Å konvertere en streng til små bokstaver"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why
Å konvertere en streng til små bokstaver kan være nyttig i situasjoner der du trenger å sammenligne eller søke etter en tilsvarende streng uten å ta hensyn til store og små bokstaver. Dette kan også være nyttig for å få en mer konsistent og enhetlig formatering av tekst.

## How To

Koden for å konvertere en streng til små bokstaver i TypeScript er veldig enkel og kan gjøres på flere forskjellige måter. Her er noen eksempler:

```TypeScript
// Metode 1: Bruke toLowerCase() metoden
let originalStreng = "EKSEMPEL";
let konvertertStreng = originalStreng.toLowerCase();
console.log(konvertertStreng);
// Output: eksempel

// Metode 2: Bruke spread operator og map funksjon
let originalStreng = "EKSEMPEL";
let konvertertStreng = [...originalStreng].map(bokstav => bokstav.toLowerCase()).join("");
console.log(konvertertStreng);
// Output: eksempel

// Metode 3: Bruke regex
let originalStreng = "EKSEMPEL";
let konvertertStreng = originalStreng.replace(/[A-Z]/g, bokstav => bokstav.toLowerCase());
console.log(konvertertStreng);
// Output: eksempel
```

Du kan også lage din egen funksjon for å konvertere strenger til små bokstaver ved å bruke en lignende tilnærming som i metode 2 og 3.

## Deep Dive

I JavaScript, som TypeScript er bygget på, er strenger uforanderlige (immutable). Dette betyr at du ikke kan endre en eksisterende streng, men heller må lage en ny streng med de ønskede endringene. Derfor vil alle metodene og eksemplene ovenfor returnere en kopi av den originale strengen som er konvertert til små bokstaver.

Det er også viktig å være klar over at konverteringen til små bokstaver skjer i henhold til språkinnstillingene til enheten eller nettleseren. Dette kan føre til forskjellige resultater i ulike språkområder.

## See Also

- [toLowerCase() metoden på MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Spread operator på MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Spread_syntax)
- [Map funksjonen på MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map)
- [Regex på MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)