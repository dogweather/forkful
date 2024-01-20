---
title:                "Generere tilfeldige tall"
html_title:           "Arduino: Generere tilfeldige tall"
simple_title:         "Generere tilfeldige tall"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Generering av tilfeldige tall er en metode for å produsere tall som ikke kan forutsies bedre enn ved en tilfeldig sjanse. Programmers bruker det for å tilføye usikkerhet i spill, simuleringer og algoritmer, samt for løse problemer som involverer tilfeldige prosesser.

## Hvordan å:
Her er noen eksempler på hvordan du kan generere et tilfeldig tall i Javascript:

```Javascript
// Genererer et tilfeldig tall mellom 0 (inkludert) og 1 (ekskludert)
let randomNumber = Math.random();
console.log(randomNumber);
```

```Javascript
// Genererer et tilfeldig heltall mellom 1 og 10
let randomInt = Math.floor(Math.random() * 10) + 1;
console.log(randomInt);
```

## Dypdykk
Historisk sett har metoder for generering av tilfeldige tall inkludert ting som å rulle terninger, snurre et hjul, og til og med komplekse matematiske beregninger. I moderne databehandling genererer vi ofte pseudotilfeldige tall ved hjelp av algoritmer som "linear congruential generators" eller "Mersenne Twister".

Alternativene til Javascripts `Math.random()` inkluderer `crypto.getRandomValues()` for kryptografisk sikre tilfeldige tall. For å generere tilfeldige heltall i et spesifikt intervall kan du lage din funksjon.

Genereringen av tilfeldige tall i Javascript er faktisk et pseudotilfeldig tall fordi det er generert med en forutsigbar algoritme. Men for de fleste formål er det "tilfeldig nok". Det er viktig å merke seg at `Math.random()` ikke er sikker nok for kryptografiske formål.

## Se Også
Hvordan lage din egen tilfeldig tallgenerator:
https://www.w3schools.com/js/js_random.asp

En oversikt over ulike metoder for å lage tilfeldige tall:
https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Numbers_and_dates

Krypto-sikre tilfeldige tall:
https://developer.mozilla.org/en-US/docs/Web/API/Web_Crypto_API/random_values