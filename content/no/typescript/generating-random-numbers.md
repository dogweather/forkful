---
title:                "Generering av tilfeldige tall"
date:                  2024-01-20T17:50:02.172550-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generering av tilfeldige tall"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Vi trenger ofte tilfeldige tall i koding for å simulere usikkerhet eller skape varierte erfaringer. Uten dem, ville spill vært forutsigbare, og sikkerhetssystemer, vel, ikke så sikre.

## How to:
TypeScript bruker JavaScripts innebygde `Math.random()` for å generere tilfeldige tall. Her er en kjapp guide.

```typescript
// Få et tilfeldig heltall mellom min (inklusiv) og max (eksklusiv)
function getRandomInt(min: number, max: number): number {
  return Math.floor(Math.random() * (max - min) + min);
}

// Få et tilfeldig desimaltall mellom 0 (inklusiv) og 1 (eksklusiv)
function getRandomArbitrary(): number {
  return Math.random();
}

// Bruk:
console.log(getRandomInt(1, 10)); // f.eks. 7
console.log(getRandomArbitrary()); // f.eks. 0.874723
```

## Deep Dive
Generere tilfeldige tall i programmering har kommet langt siden de tidlige datamaskinene. Maskinvareløsninger, som støybaserte eller fysiske tilfeldige tallgeneratorer, har gitt vei for softwarebaserte metoder som brukes i dag. 

`Math.random()` i JavaScript (og TypeScript) er grei for mange bruksområder, men den er egentlig ikke helt tilfeldig; det er en pseudo-tilfeldig tallgenerator (PRNG). For oppgaver som krever høyt sikkerhetsnivå eller vitenskapelig nøyaktighet, vender utviklere seg til mer avanserte algoritmer som Mersenne Twister eller crypto-biblioteket for kryptografisk sikre tall.

Det er viktig å merke seg at `Math.random()` i JavaScript ikke lar deg sette en egen frøverdi (seed). Dette begrenser bruken hvis du trenger reproduserbare tilfeldige sekvenser. Noen bibliotek og rammeverk tilbyr denne funksjonaliteten, så det kan være verdt å lete etter tredjepartsalternativer hvis ditt prosjekt krever det.

## See Also
- MDN Web Docs, Math.random: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- Mersenne Twister algoritme: [https://en.wikipedia.org/wiki/Mersenne_Twister](https://en.wikipedia.org/wiki/Mersenne_Twister)
- Node.js crypto modul: [https://nodejs.org/api/crypto.html](https://nodejs.org/api/crypto.html)
- 'seedrandom' pakken for reproduserbare tilfeldige tall i JavaScript: [https://github.com/davidbau/seedrandom](https://github.com/davidbau/seedrandom)
