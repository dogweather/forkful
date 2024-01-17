---
title:                "Generering av tilfeldige tall"
html_title:           "Javascript: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Generering av tilfeldige tall er en viktig del av programmering. Det er en måte for utviklere å lage enkelt og komplekst innhold på en tilfeldig måte, som kan være nyttig for mange forskjellige oppgaver.

## Slik gjør du:
For å generere tilfeldige tall i Javascript, kan du bruke funksjonen ```Math.random()```. Denne funksjonen returnerer et tilfeldig tall mellom 0 og 1. For å få et tilfeldig tall innenfor et spesifikt område, kan du bruke formlene:  
```
Math.floor(Math.random() * (max - min)) + min;
```
Her er et eksempel på kode som genererer et tilfeldig tall mellom 1 og 10 og skriver det ut i konsollen:  
```
let randomNum = Math.floor(Math.random() * (10 - 1)) + 1;
console.log(randomNum);
```
Dette vil for eksempel gi resultatet 7.

## Dypdykk:
Generering av tilfeldige tall har vært en nødvendighet i programmering siden de tidlige dagene. I begynnelsen ble tilfeldige tall opprettet ved hjelp av kompliserte algoritmer, men med utviklingen av matematiske funksjoner som ```Math.random()``` er det nå enklere å generere tilfeldige tall.

En annen måte å generere tilfeldige tall på er ved hjelp av tredjeparts biblioteker som tilbyr mer avanserte funksjoner som å lage tilfeldige datoer, strenger eller til og med liste over tilfeldige tall.

## Se også:
- [MDN dokumentasjon for Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [JS Random Library](https://www.npmjs.com/package/js.random) (en tredjeparts "random" bibliotek for Javascript) 
- [Random.org](https://www.random.org/) (en online tjeneste for ekte tilfeldige nummergenerering)