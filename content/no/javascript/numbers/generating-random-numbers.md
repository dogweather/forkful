---
date: 2024-01-27 20:34:14.904248-07:00
description: "\xC5 generere tilfeldige tall i JavaScript er en teknikk som brukes\
  \ for \xE5 skape uforutsigbarhet i applikasjoner, fra spill som trenger tilfeldig\
  \ fiendeadferd\u2026"
lastmod: '2024-03-13T22:44:41.179886-06:00'
model: gpt-4-0125-preview
summary: "\xC5 generere tilfeldige tall i JavaScript er en teknikk som brukes for\
  \ \xE5 skape uforutsigbarhet i applikasjoner, fra spill som trenger tilfeldig fiendeadferd\
  \ til sikkerhetsalgoritmer som krever kryptografisk tilfeldighet."
title: Generering av tilfeldige tall
weight: 12
---

## Hvordan:


### Grunnleggende generering av tilfeldige tall
Den mest direkte måten å generere et tilfeldig tall i JavaScript på, er å bruke `Math.random()`. Denne funksjonen returnerer et flyttall, pseudo-tilfeldig tall i området 0 (inklusiv) til 1 (eksklusiv).

```javascript
let randomNumber = Math.random();
console.log(randomNumber);
```

### Generere et tilfeldig tall innenfor et område
Ofte vil du ha et tilfeldig heltall innenfor et spesifikt område. Dette kan oppnås ved å skalere og runde av utdataen fra `Math.random()`.

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

console.log(getRandomInt(1, 100));
```

### Kryptografisk sikre tilfeldige tall
For applikasjoner som krever en høyere grad av tilfeldighet (f.eks. kryptografiske operasjoner), kan metoden `crypto.getRandomValues()` brukes. Dette gir kryptografisk tilfeldighet, i motsetning til de pseudo-tilfeldige tallene som genereres av `Math.random()`.

```javascript
(function generateSecureRandom() {
  let array = new Uint32Array(1);
  window.crypto.getRandomValues(array);
  console.log(array[0]);
})();
```

## Dypdykk
Historisk sett har generering av tilfeldige tall i JavaScript utelukkende vært avhengig av funksjonen `Math.random()`. Selv om den er praktisk for de fleste tilfeldige bruksområder, gir algoritmen den bruker, typisk en variant av en pseudotilfeldig tallgenerator (PRNG) som Mersenne Twister, ikke kryptografisk sikkerhet.

Introduksjonen av Web Cryptography API brakte med seg metoden `crypto.getRandomValues()`, som tilbyr en måte å generere tall som er langt mindre forutsigbare og egnet for sikkerhetssensitive applikasjoner. Denne metoden utnytter kildene til tilfeldighet i det underliggende operativsystemet, som `/dev/random` på Unix/Linux, som er mer robuste og egnet for kryptografiske operasjoner.

Det er avgjørende å velge riktig metode for oppgaven som skal utføres. `Math.random()` er tilstrekkelig for grunnleggende behov som enkle spill, animasjoner, eller ethvert tilfelle der kvaliteten på tilfeldigheten ikke er kritisk. Imidlertid, for sikkerhetsfunksjoner, som tilbakestilling av passordtoken eller enhver kryptografisk operasjon, er `crypto.getRandomValues()` det bedre valget på grunn av dens overlegne tilfeldighetskvalitet.

Merkbart er at `Math.random()` genererer tall med en kjent skjevhet i de fleste implementasjoner, noe som betyr at noen tall er mer sannsynlige å forekomme enn andre. Selv om denne skjevheten er minimal og ofte uoppdagelig for generelle applikasjoner, diskvalifiserer den `Math.random()` fra å bli brukt i enhver kryptografisk kontekst eller applikasjoner hvor rettferdighet er kritisk, slik som online gambling.

Konklusjonen er at mens JavaScripts innebygde funksjoner for å generere tilfeldige tall dekker et bredt spekter av behov, er forståelse av forskjellene og begrensningene av hver metode essensiell for deres passende anvendelse.
