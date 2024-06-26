---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:09.076841-07:00
description: "Hvordan: I Google Apps Script kan logging utf\xF8res ved hjelp av ulike\
  \ metoder, som `Logger`-klassen og `console.log()`. Logger-klassen er den tradisjonelle\u2026"
lastmod: '2024-03-13T22:44:40.322695-06:00'
model: gpt-4-0125-preview
summary: "I Google Apps Script kan logging utf\xF8res ved hjelp av ulike metoder,\
  \ som `Logger`-klassen og `console.log()`."
title: Logging
weight: 17
---

## Hvordan:
I Google Apps Script kan logging utføres ved hjelp av ulike metoder, som `Logger`-klassen og `console.log()`. Logger-klassen er den tradisjonelle måten, egnet for enkel feilsøking og utviklingsformål. Med de siste oppdateringene, tilbyr `console.log()` mer fleksibilitet og integrasjon med Stackdriver Logging, og gir en mer robust løsning for overvåking av dine Apps Scripts i Google Cloud Platform.

**Bruke Logger:**

```javascript
function logSample() {
  Logger.log('Dette er en enkel loggmelding');
  
  var verdi = 5;
  Logger.log('Verdien er: %s', verdi); // Strengformatering
}

// For å se loggen:
// 1. Kjør logSample-funksjonen.
// 2. Vis -> Logger
```

**Eksempel på Logger-utdata:**

```
[22-04-20 10:00:00:000 PDT] Dette er en enkel loggmelding
[22-04-20 10:00:00:001 PDT] Verdien er: 5
```

**Bruke console.log():**

```javascript
function consoleLogSample() {
  console.log('Denne meldingen går til Stackdriver Logging');
  const obj = {navn: 'Jane', rolle: 'Utvikler'};
  console.info('Logger et objekt:', obj);
}

// Logger kan vises i Google Cloud Platform (GCP)-konsollen under Stackdriver Logging
```

**Eksempel på console.log()-utdata:**

```
Denne meldingen går til Stackdriver Logging
Logger et objekt: {navn: "Jane", rolle: "Utvikler"}
```

Ved å gå over til `console.log()` for komplekse applikasjoner, kan utviklere effektivt parse og analysere logger ved hjelp av de kraftige filtrene og verktøyene som tilbys av GCP, noe som ikke er så rett frem med den tradisjonelle Logger-klassen.

## Dypdykk:
Logging i Google Apps Script har utviklet seg betydelig. I begynnelsen var `Logger`-klassen den primære metoden for utviklere å feilsøke skriptene sine. Den er enkel og tilstrekkelig for grunnleggende skript, men mangler evnene som trengs for moderne skyapplikasjoner, som å søke i logger eller analysere loggtrender over tid.

Introduksjonen av `console.log()` tettet dette gapet ved å integrere logging i Google Apps Script med Google Clouds Stackdriver Logging (nå kalt Operations Suite), som gir en sentralisert plattform for logging, overvåking og feilsøking av applikasjoner. Dette tillot ikke bare logging i stor skala, men åpnet også for avanserte funksjoner for loggstyring som logg-baserte metrikker, analyse av logger i sanntid og integrasjon med andre Google Cloud-tjenester.

Selv om `Logger` fortsatt tjener et formål for rask feilsøking og logging i mindre skript, reflekterer overgangen til å bruke `console.log()` en bredere endring i utviklingen av skalerbare, sky-native applikasjoner. Det understreker Googles engasjement for å gi utviklere verktøy som tar hensyn til kompleksiteten og skalaen til dagens applikasjoner. Imidlertid bør nykommere være oppmerksomme på den litt brattere læringskurven og nødvendigheten av å gjøre seg kjent med Google Cloud Platform-konsepter. Til tross for dette, er overgangen fordelaktig for utviklere som ser etter å fullt ut utnytte skykapasiteter. Denne justeringen med skytjenester er del av en større trend i programvareutvikling, som understreker viktigheten av robuste, skalerbare loggingsmekanismer i skydatabehandlingens tidsalder.
