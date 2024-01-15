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

## Hvorfor

Å generere tilfeldige tall kan være nyttig i mange programmeringsscenarier. Det kan for eksempel brukes til å lage unike brukernavn, tilfeldige farger i et spill eller for å sikre at en funksjon blir utført tilfeldig.

## Hvordan

For å generere tilfeldige tall i Javascript kan vi bruke Math.random() -funksjonen. Denne funksjonen returnerer et desimaltall mellom 0 og 1. For å få et heltall kan vi multiplisere dette tallet med et ønsket antall og bruke Math.floor() -funksjonen for å runde det ned til nærmeste heltall.

```Javascript
// Eksempel på å generere et tilfeldig tall mellom 1 og 10
let tilfeldigTall = Math.floor(Math.random() * 10) + 1;
console.log(tilfeldigTall); // Output: et tilfeldig tall mellom 1 og 10
```

Vi kan også bruke Math.ceil() -funksjonen for å runde tallet opp, eller Math.round() -funksjonen for å runde det til nærmeste heltall.

For å generere et tilfeldig tall innenfor et bestemt intervall, for eksempel mellom 20 og 50, kan vi bruke følgende kode:

```Javascript
// Eksempel på å generere et tilfeldig tall mellom 20 og 50
let tilfeldigTall = Math.floor(Math.random() * (50 - 20 + 1)) + 20;
console.log(tilfeldigTall); // Output: et tilfeldig tall mellom 20 og 50
```

## Dykk dypere

Ved å bruke Math.random() -funksjonen, vil tallene vi genererer alltid være tilfeldige, men de vil ikke nødvendigvis være helt tilfeldige. Dette skyldes at funksjonen er basert på en algoritme og påvirkes av faktorer som datamaskinens klokke og kjørende prosesser.

For å få enda mer tilfeldige tall kan vi bruke et såkalt "seed" eller utgangspunkt. Dette er et tall vi gir til Math.random() -funksjonen og som vil påvirke den genererte sekvensen av tall. For å gjøre dette, kan vi bruke en tredjepartsbibliotek som "random-seed" eller "seed-random", som gir oss muligheten til å bruke et seed i Math.random() -funksjonen.

## Se også

- [MDN Web Docs: Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [The Modern Javascript Tutorial: Generating random numbers in a given range](https://javascript.info/task/random-int-min-max)