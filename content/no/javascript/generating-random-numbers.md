---
title:                "Generering av tilfeldige tall"
date:                  2024-01-20T17:49:16.967918-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generering av tilfeldige tall"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å generere tilfeldige tall er en måte å skape verdier som er uforutsigbare. Programmerere gjør dette for simuleringer, spill, sikkerhetstester og der variabilitet er nødvendig.

## Hvordan:
```javascript
// Enkel tilfeldig heltall mellom 0 (inkludert) og 10 (ekskludert)
let randomNum = Math.floor(Math.random() * 10);
console.log(randomNum); // Output kan være et hvilket som helst tall mellom 0 og 9

// Tilfeldig tall mellom 1 og 100
let randomNumBetween1And100 = Math.floor(Math.random() * 100) + 1;
console.log(randomNumBetween1And100); // Output: 1-100

// Tilfeldig flyttall mellom 0 (inkludert) og 1 (ekskludert)
let randomFloat = Math.random();
console.log(randomFloat); // Output: f.eks. 0.123456789
```

## Dypdykk
Generering av tilfeldige tall i JavaScript er en ofte undervurdert kunst. Metoden `Math.random()` ble introdusert i ECMAScript 1 (1997), som grunnlaget for tilfeldigheter. Selv om den gir pseudotilfeldige tall, er den god nok for mange tilfeller, men ikke for kryptografiske formål.

Alternativer inkluderer crypto-biblioteket for mer sikre tilfeldige tall, eller tredjepart bibliotek som `chance.js` hvis du trenger mer funksjonalitet.

Det er også viktig å vite at `Math.random()` genererer et tall fra et pseudotilfeldig tall-generator (PRNG) algoritme; det er ikke egentlig "ekte" tilfeldighet, men det imiterer det gjennom komplekse beregninger.

## Se Også
- MDN Web Docs on Math.random(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random
- Chance.js for more advanced random generation: http://chancejs.com
- Node Crypto module for cryptographic randomness: https://nodejs.org/api/crypto.html
