---
title:                "Generere tilfeldige tall"
aliases:
- /no/google-apps-script/generating-random-numbers/
date:                  2024-02-01T21:54:07.667695-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generere tilfeldige tall"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/google-apps-script/generating-random-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å generere tilfeldige tall er en grunnleggende oppgave i programmering som brukes til en rekke applikasjoner, som simuleringer, spill og sikkerhetssystemer. Programmerere bruker denne teknikken i Google Apps Script for å introdusere variabilitet, teste scenarier og legge til uforutsigbarhet i applikasjonene sine innenfor Google-økosystemet, inkludert Sheets, Docs og Forms.

## Hvordan:

I Google Apps Script kan du generere tilfeldige tall ved å bruke `Math.random()`-funksjonen, lik JavaScript. Denne funksjonen returnerer et flyttall, pseudo-tilfeldig tall i området 0 (inklusivt) til 1 (eksklusivt). For å tilpasse disse tallene for ulike bruksområder, som å generere heltall innenfor et spesifikt område, kan det være nødvendig med ytterligere beregninger.

### Generere et grunnleggende tilfeldig tall

For å generere et enkelt tilfeldig tall og logge det til konsollen:

```javascript
function generateRandomNumber() {
  var randomNumber = Math.random();
  Logger.log(randomNumber);
}
```
*Eksempelutdata:* `0.1234567890123456`

### Generere et heltall innenfor et spesifikt område

For å generere et tilfeldig heltall mellom to verdier (`min` og `max`), inklusivt:

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  var randomNumber = Math.floor(Math.random() * (max - min + 1)) + min;
  Logger.log(randomNumber);
  return randomNumber;
}

// Eksempel:
getRandomInt(1, 10);
```
*Eksempelutdata*: `7`

Husk at `Math.ceil()`-funksjonen brukes til å runde minimumsverdien opp, og `Math.floor()` brukes til å runde maksimumsverdien ned, noe som sikrer at det tilfeldige tallet er innenfor det spesifiserte området.

## Dypdykk

Mekanismen for å generere tilfeldige tall i Google Apps Script, og faktisk i de fleste programmeringsspråk, benytter en pseudo-tilfeldig tallgenerator (PRNG). Denne teknikken er deterministisk og stoler på en initialverdi, kjent som frøet, for å produsere en sekvens av tall som ser tilfeldige ut. Selv om dette er tilstrekkelig for mange applikasjoner, er det viktig å merke seg at pseudo-tilfeldige tall kanskje ikke er passende der høy sikkerhet eller ekte tilfeldighet er nødvendig, som i kryptografiske applikasjoner.

Ekte tilfeldighet kan oppnås gjennom maskinvare tilfeldig tallgeneratorer eller tjenester som genererer tilfeldighet fra naturlige fenomener. Imidlertid, for de fleste daglige skriptbehov i Google Apps Script, holder `Math.random()`.

Historisk sett har jakten på mer effektive teknikker for generering av tilfeldige tall ført til utviklingen av ulike algoritmer, med kjente eksempler som Mersenne Twister og Linear Congruential Generator (LCG). Likevel, gitt det høye nivået av abstraksjon i Google Apps Script, trenger de fleste brukere ikke å implementere disse algoritmene direkte, men å forstå de underliggende prinsippene kan hjelpe i å sette pris på betydningen og begrensningene av tilfeldig tallgenerering i skriptene dine.
