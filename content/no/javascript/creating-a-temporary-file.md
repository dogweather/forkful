---
title:                "Lage en midlertidig fil"
html_title:           "Javascript: Lage en midlertidig fil"
simple_title:         "Lage en midlertidig fil"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lage midlertidige filer er en vanlig teknikk i programmering, spesielt i Javascript. Det kan være nyttig når du trenger å lagre midlertidige data eller operere på store datasett uten å påvirke den opprinnelige filen.

## Hvordan

Oppretting av en midlertidig fil i Javascript kan gjøres ved å bruke fs modulen. Før du kan gjøre det, må du inkludere modulen i koden din ved å skrive ```const fs = require('fs')``` øverst i filen.

Deretter kan du bruke ```fs.openSync()``` funksjonen for å opprette en fil. Denne funksjonen tar inn to parametere, filnavn og en flaggparameter som indikerer handlingen som skal utføres med filen. For å opprette en midlertidig fil, bruker du flagget ```'w'```.

Etter å ha opprettet filen, kan du skrive data til den ved hjelp av ```fs.writeSync()``` funksjonen. Dette vil tillate deg å skrive til filen på en lignende måte som du ville gjort med vanlige filer.

Etter å ha fullført operasjonene dine, må du huske å lukke filen ved å bruke ```fs.closeSync()``` funksjonen. Dette vil sørge for at midlertidig fil slettes når programmet avsluttes.

Et eksempel på hvordan du oppretter og skriver data til en midlertidig fil:

```Javascript
const fs = require('fs')

// Oppretter en midlertidig fil
fs.openSync('midlertidig.txt', 'w')

// Skriver data til filen
fs.writeSync('midlertidig.txt', 'Dette er midlertidige data.')

// Lukker filen
fs.closeSync('midlertidig.txt')
```

Etter å ha kjørt denne koden, vil du se at en ny fil med navnet "midlertidig.txt" er opprettet og den vil inneholde teksten "Dette er midlertidige data.".

## Dypdykk

Når du oppretter en midlertidig fil, er det viktig å huske at filen kun vil eksistere så lenge programmet kjører. Når programmet avsluttes, vil filen bli slettet automatisk. Dette er grunnen til at det er viktig å være forsiktig med å bruke midlertidige filer, spesielt når det gjelder lagring av viktig data.

En annen ting å merke seg er at navnet på den midlertidige filen kan være tilfeldig tildelt av operativsystemet. Så sørg for å ikke basere logikken din på å ha et bestemt navn på filen, men heller på å bruke operasjoner som å lese og skrive data til filen.

## Se også

- [Node.js fs modulen](https://nodejs.org/api/fs.html)
- [W3Schools guide om hvordan du bruker fs i Node.js](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)
- [En guide til å håndtere midlertidige filer i Javascript](https://medium.com/@KaweesiJoseph/working-with-temporary-files-in-node-js-be6d5c75c851)