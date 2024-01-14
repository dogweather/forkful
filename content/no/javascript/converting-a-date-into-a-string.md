---
title:                "Javascript: Konvertering av dato til streng"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Konvertering av datoer til tekststrenger er en viktig funksjon når man jobber med datointervaller, kalendere eller tidssoner. Det gjør det enklere å presentere datoer på en forståelig måte for brukeren, og å sammenligne datoer på en mer presis måte i koden.

## Hvordan å gjøre det
```Javascript
// Her er et eksempel på hvordan man kan konvertere en dato til en tekststreng
let date = new Date(); // Oppretter en ny datoobjekt
let stringDate = date.toDateString();
console.log(stringDate); // Output: Mon Aug 23 2021

// Man kan også bruke den mer presise metoden .toLocaleDateString() som tar med informasjon om tiden og tidssonen
let localeDateString = date.toLocaleDateString('en-US', { timeZone: 'UTC' });
console.log(localeDateString); // Output: 8/23/2021
```

## Dypdykk
Det finnes flere metoder for å konvertere datoer til tekststrenger i JavaScript. Noen av disse inkluderer .toUTCString(), .toGMTString(), og .toISOString(). Disse metodene har litt forskjellig utgangspunkt, og det kan være viktig å velge den som passer best til ditt brukstilfelle. Det er også mulig å formatere datoene på spesifikke måter ved å bruke .toLocaleString() og tilhørende metoder, som .toLocaleDateString() og .toLocaleTimeString().

## Se også
- MDN Web Docs: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toDateString
- W3Schools: https://www.w3schools.com/jsref/jsref_todatestring.asp
- FreeCodeCamp: https://www.freecodecamp.org/news/javascript-converting-dates-to-strings/