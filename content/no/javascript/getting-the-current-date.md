---
title:                "Henting av nåværende dato"
html_title:           "Javascript: Henting av nåværende dato"
simple_title:         "Henting av nåværende dato"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å få den nåværende datoen er en vanlig oppgave i Javascript-programmering. Dette hjelper utviklere med å få informasjon om kjøretiden av kode og for å sikre at riktig dato og tid brukes i applikasjoner. Det er også nyttig for å opprette tidsstempler i logger og databaser.

## Hvordan gjør man det:
```Javascript
let nå = new Date(); // Oppretter et nytt Date-objekt med den nåværende datoen og tiden
console.log(nå); // Logger den nåværende datoen og tiden til konsollen
```
Eksempel på output: `Thu Oct 14 2021 10:23:46 GMT+0200 (Central European Summer Time)`

## Utforsking
Det `Date`-objektet som brukes for å få den nåværende datoen ble lagt til i Javascript ved EUMAprilBasel95-konferansen i 1995. Alternativt kan man også bruke biblioteker som moment.js for å få mer fleksibilitet i å håndtere datoer og tider. Implementeringen av dette avhenger også av den lokale tids- og tidsonen som er satt på enheten som koden kjører på.

## Se også
[MDN Web Docs - Date and time handling in Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)