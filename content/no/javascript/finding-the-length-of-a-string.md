---
title:                "Javascript: Å finne lengden av en tekststreng"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
I denne bloggposten skal vi utforske hvordan man kan finne lengden til en tekststreng i Javascript. Enten du er nybegynner eller en erfaren programmerer, er det alltid nyttig å kunne håndtere og manipulere strenger på en effektiv måte. Å kjenne til lengden på en streng er en grunnleggende ferdighet som kan være nyttig i mange ulike situasjoner. Enten det er for å validere brukerinput, formatere tekst eller gjøre beregninger basert på strenglengden, er det viktig å kunne finne lengden til en streng på en enkel og effektiv måte.

## Slik gjør du det
For å finne lengden til en streng i Javascript, kan vi bruke metoden `length` på et variabelnavn som inneholder en streng. La oss se på et eksempel for å forstå hvordan dette fungerer:

```Javascript
let tekst = "Hei, verden!";
console.log(tekst.length); // Output: 12
```

Som vi ser, returnerer metoden `length` verdien 12, som er antall tegn i strengen "Hei, verden!". Det er viktig å merke seg at metoden ikke inkluderer mellomrom når den teller antall tegn i en streng. La oss se på et annet eksempel for å demonstrere dette:

```Javascript
let tekst = "Dette er en lang tekst.";
console.log(tekst.length); // Output: 23
```

Vi ser at selv om det er 24 tegn i strengen, returnerer metoden `length` verdien 23, siden den ikke tar med mellomrommet i tellingen.

## I dybden
Det kan være lurt å vite at metoden `length` ikke bare fungerer på tekststrenger, men også på andre datatyper som for eksempel arrays. Den teller da antall elementer i arrayet. Dette kan være nyttig når du jobber med lister og trenger å vite antall elementer i listen.

En annen viktig ting å være klar over er at metoden `length` returnerer et tall som er basert på indeksering. Dette betyr at den første indeksen i en streng eller array er 0, og derfor vil lengden være en høyere verdi enn indeksen til det siste elementet. Dette kan være til hjelp når du jobber med løkker og behandler hver enkelt indeks.

## Se også
- [MDN: String.prototype.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [W3Schools: JavaScript String length Property](https://www.w3schools.com/jsref/jsref_length_string.asp)
- [Stack Overflow: What is the way to get string length in JavaScript?](https://stackoverflow.com/questions/26229791/what-is-the-way-to-get-string-length-in-javascript)