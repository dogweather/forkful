---
title:                "Ekstrahering av delstrenger"
html_title:           "Javascript: Ekstrahering av delstrenger"
simple_title:         "Ekstrahering av delstrenger"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Utvinning av delstrenger er en vanlig oppgave i programmering hvor man tar ut en del av en større streng for videre behandling. Dette kan være nyttig når man ønsker å manipulere tekst eller hente ut spesifikke deler av data.

## Slik gjør du det:
For å utvinne en delstreng bruker vi metoden `substring()` i Javascript. Denne metoden tar to parametere, startindeks og sluttindeks, og returnerer en del av strengen basert på disse verdiene. Her er et eksempel:

```Javascript
let tekst = "Dette er en tekst";
let delstreng = tekst.substring(5, 10);

console.log(delstreng); // output: er en
```

Vi kan også bruke negative tall som indekser, hvor `-1` representerer siste indeks i strengen. Dette er nyttig når man ikke vet lengden på strengen. Se neste eksempel:

```Javascript
let tekst = "Dette er en tekst";
let delstreng = tekst.substring(7, -3);

console.log(delstreng); // output: en
```

## Dypdykk:
Metoden `substring()` ble introdusert i ECMAScript versjon 1, som er den første versjonen av Javascript. Den har nå blitt erstattet av metoden `slice()`, som fungerer på samme måte, men hvor man også kan bruke negative indekser som enklere kan representere siste indeks i strengen.

En annen måte å utvinne delstrenger i Javascript på er ved hjelp av `substr()` metoden, som tar to parametere, startindeks og lengde, og returnerer en del av strengen basert på disse verdiene.

## Se også:
- [MDN Web Docs - substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN Web Docs - slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN Web Docs - substr()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr)