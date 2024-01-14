---
title:    "TypeScript: Generering av tilfeldige tall"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Å generere tilfeldige tall er en viktig funksjon i mange programmeringsprosjekter. Enten det er å lage et dataspill med ulike nivåer av vanskelighetsgrad eller å utføre statistiske analyser, kan tilfeldige tall bidra til å gjøre programmet mer variert og realistisk. I denne bloggposten vil vi se på hvordan vi kan lage tilfeldige tall i TypeScript.

## Hvordan

Vi kan enkelt generere tilfeldige tall i TypeScript ved å bruke funksjonen ```Math.random()```. Denne funksjonen returnerer et tilfeldig tall mellom 0 og 1. Ved å gange dette tallet med ønsket øvre grense og deretter rundet av til nærmeste heltall, kan vi få et tilfeldig tall innenfor ønsket område.

For eksempel, hvis vi vil ha et tilfeldig tall mellom 1 og 10:

```TypeScript
let randomNum = Math.random() * 10;
randomNum = Math.round(randomNum) + 1;
console.log(randomNum); // Output: et tilfeldig tall mellom 1 og 10
```

Vi kan også bruke en lignende tilnærming for å få tilfeldige tegn i en streng ved å bruke funksjonen ```String.fromCharCode()```.

```TypeScript
let randomChar = String.fromCharCode(Math.floor(Math.random() * 26) + 97);
console.log(randomChar); // Output: et tilfeldig tegn fra a til z
```

## Dypdykk

Det er viktig å merke seg at funksjonen ```Math.random()``` ikke genererer ekte tilfeldige tall, da det er basert på en matematisk algoritme. For å få mer virkelighetsnære tilfeldige tall, kan vi bruke eksterne biblioteker som genererer tall basert på ekte fysiske fenomener, som radioaktiv nedbrytning eller atmosfæriske støy.

Vi kan også kontrollere inndataene til ```Math.random()``` ved å tilbakestille den innbygde pseudotilfeldighetsgeneratoren (PRNG) ved hjelp av funksjonen ```Math.seedrandom()```, som lar oss sette en startverdi for genereringen av tilfeldige tall. Dette kan være nyttig for å skape gjentagende tilfeldige tall for testing og debugging formål.

## Se også

- [MDN Web Docs - Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [RandomNumberAPI](https://www.randomnumberapi.com/)
- [GitHub - seedrandom](https://github.com/davidbau/seedrandom)