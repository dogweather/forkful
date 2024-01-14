---
title:    "TypeScript: Uttrekking av substringer"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Substrings er en viktig del av programmering, spesielt innen TypeScript. Å ekstrahere substrings betyr å finne og isolere en del av en streng (tekst) og bruke den i koden din. Dette kan være nyttig for å organisere og håndtere data, samt for å formatere og manipulere tekst på en enkel måte.

## Slik gjør du det

For å ekstrahere substrings i TypeScript, bruker du funksjonene `substr()` og `substring()`. Begge disse funksjonene tar inn to argumenter: startindeks og sluttindeks for den ønskede delen av strengen.

```
//Eksempel på hvordan bruke `substr()`:
let minTekst = "Dette er en eksempeltekst";

//Ekstrahere "eksempeltekst":
let delStreng = minTekst.substr(14);

//Eksempel på hvordan bruke `substring()`:
//Ekstrahere "en":
let delStreng = minTekst.substring(9, 11);

console.log(delStreng); //Output: "en"
```

I tillegg til `substr()` og `substring()`, kan du også bruke operatorer som `slice()` og `split()` til å ekstrahere substrings. Disse funksjonene fungerer på samme måte som `substr()` og `substring()`, men gir deg mer kontroll og fleksibilitet når du jobber med strenger.

## Dypdykk

En viktig ting å huske på når du arbeider med substrings, er at de bruker zero-based indexering. Dette betyr at den første bokstaven i en streng har indeks 0, den andre bokstaven har indeks 1, og så videre. Derfor vil for eksempel `minTekst.substr(0, 3)` gi deg de første tre bokstavene i strengen, ikke de første tre ordene.

Det finnes også ulike måter å angi start- og sluttindeks på, avhengig av hvilken funksjon du bruker. For eksempel kan du bruke negative tall for å telle baklengs fra slutten av strengen, eller bruke en andre streng som referanse for å angi indeksene.

Det er også viktig å merke seg at substrings er "pass by reference", noe som betyr at de endrer den opprinnelige strengen de er ekstrahert fra. Hvis du ikke ønsker å endre den opprinnelige strengen, kan du i stedet bruke `slice()` eller `split()`.

## Se også

- [String manipulation in TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html)
- [JavaScript Substring, substr and substring](https://www.w3schools.com/jsref/jsref_substr.asp)
- [JavaScript String slice() method](https://www.w3schools.com/jsref/jsref_slice_string.asp)
- [JavaScript split() method](https://www.w3schools.com/jsref/jsref_split.asp)