---
title:                "Generering av tilfeldige tall"
html_title:           "TypeScript: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Generering av tilfeldige tall er en metode som lar programmerere lage en verdi som ikke følger et forhåndsbestemt mønster. Dette er nyttig for å introdusere tilfeldighet og variasjon i programmer, for eksempel i spill og simuleringer.

## Slik gjør du:
Slumpetall-biblioteket i TypeScript lar deg enkelt generere tilfeldige tall ved å bruke ```Math.random()``` funksjonen. Dette vil gi deg et tall mellom 0 og mindre enn 1. For å få et helt tall i et gitt intervall kan du bruke følgende formel: ```Math.floor(Math.random() * (max - min + 1)) + min```. Her vil variablene ```min``` og ```max```representere de laveste og høyeste verdiene du ønsker å generere.

## Dypdykk:
Generering av tilfeldige tall har en lang historie og har vært grunnlaget for flere komplekse matematiske teorier. Selv om ```Math.random()``` funksjonen er enkel og effektiv, finnes det også alternative metoder som kan gi enda jevnere fordeling av tallene. Det finnes også biblioteker som spesialiserer seg på å generere tilfeldige tall for ulike formål, for eksempel simuleringer og kryptografi. 

## Se også:
- [MDN web docs - Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Random.js - bibliotek for generering av tilfeldige tall](https://github.com/davidbau/random-js)