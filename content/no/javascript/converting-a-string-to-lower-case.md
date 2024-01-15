---
title:                "Konvertere en streng til små bokstaver."
html_title:           "Javascript: Konvertere en streng til små bokstaver."
simple_title:         "Konvertere en streng til små bokstaver."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor 
Enhver programvareutvikler vet at det er nødvendig å håndtere tekststrenger i koden sin. I noen tilfeller kan det være nyttig å konvertere en tekststreng til små bokstaver, enten for sammenligning eller for å tilpasse en bestemt formatering. I denne artikkelen skal vi dykke inn i hvordan du enkelt kan konvertere en tekststreng til små bokstaver i JavaScript.

## Slik gjør du det
Konvertering av en tekststreng til små bokstaver er veldig enkelt i JavaScript, takket være den innebygde funksjonen `toLowerCase()`. Her er et eksempel på hvordan du kan bruke denne funksjonen i koden din:

```Javascript
let tekst = "HELLO, WORLD!"
let konvertertTekst = tekst.toLowerCase();

console.log(konvertertTekst);
```
Output: `hello, world!`

Som du kan se, bruker vi bare `toLowerCase()` på tekststrengen vår og lagrer den konverterte strengen i en ny variabel. Du kan nå bruke denne konverterte strengen til å sammenligne eller formatere på ønsket måte.

## Dypdykk
Det er verdt å merke seg at `toLowerCase()` funksjonen konverterer alle bokstaver i en tekststreng til små bokstaver. Dette inkluderer ikke bare alfabeter, men alle tegn som er en del av Unicode-tegnsamlingen. Dette betyr at selv om en bokstav i ditt språk kan ha en stor og liten variant, vil begge bli konvertert til små bokstaver.

Det er også viktig å merke seg at `toLowerCase()` ikke endrer den opprinnelige tekststrengen, men heller returnerer en ny konvertert versjon av den. Det er derfor vi lagrer den konverterte strengen i en ny variabel i eksempelet vårt ovenfor.

Med det sagt, er det viktig å bruke denne funksjonen forsiktig og være oppmerksom på hvilken type tekststrenger du bruker den på. Noen språk har egne funksjoner for å konvertere til små bokstaver, som for eksempel `strToLower()` i PHP.

## Se også
- [JavaScript String toLowerCase() funksjonen - MDN Web Docs](https://developer.mozilla.org/nb/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [PHP strToLower() funksjonen - PHP Manualen](https://www.php.net/manual/en/function.strtolower.php)