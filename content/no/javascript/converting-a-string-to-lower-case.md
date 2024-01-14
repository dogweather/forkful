---
title:                "Javascript: Konvertere en streng til små bokstaver"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å konvertere en streng til små bokstaver kan være nyttig i mange tilfeller. Det kan hjelpe med å standardisere data, gjøre søkefunksjoner mer fleksible, og skape en jevnere og mer konsistent tekststruktur.

## Hvordan gjøre det

For å konvertere en streng til små bokstaver, kan du bruke metoden `.toLowerCase()`. Denne metoden tar den eksisterende strengen og returnerer den samme strengen, men med alle bokstavene omgjort til små bokstaver.

```javascript
let tekst = "Ja, Jeg Vil Konvertere";
let nyTekst = tekst.toLowerCase();

console.log(nyTekst); // ja, jeg vil konvertere
```

Som du kan se i eksempelet ovenfor, returnerer `.toLowerCase()`-metoden en ny streng med alle bokstavene i små bokstaver. Den originale strengen blir ikke endret.

Dette kan også brukes på variabler som inneholder tall eller andre typer data, men det er viktig å huske at metoden kun fungerer for å konvertere bokstaver til små bokstaver, og ikke tall til bokstaver.

```javascript
let tall = 123;
let tekst = "Tekst med Tall";

console.log(tall.toLowerCase()); // Vil gi en feilmelding, da .toLowerCase() kun fungerer på strenger
console.log(tekst.toLowerCase()); // tekst med tall
```

## Dypdykk

Som nevnt tidligere, kan konvertering av strenger til små bokstaver hjelpe med å standardisere data og gjøre søkefunksjoner mer fleksible. Dette er spesielt nyttig når du må sammenligne tekststrenger, da store og små bokstaver ofte kan føre til feil i søk og sammenligninger.

Det er også verdt å nevne at `.toLowerCase()`-metoden kun fungerer for konvertering til små bokstaver i det unicode-området som omfatter de latinske bokstavene, spesielt a til z. For andre språk som bruker andre bokstaver, kan det være nødvendig å bruke andre metoder for å konvertere til små bokstaver.

## Se også

- [JavaScript string `.toLowerCase()` metode (MDN)](https://developer.mozilla.org/nb/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Unicode Character Table](https://unicode-table.com/en/) for å se hvordan forskjellige bokstaver håndteres i utf-8
- [Text Cleanse: Fra store bokstaver til små bokstaver med JavaScript](https://www.textcleanse.com/en/Lowercase)