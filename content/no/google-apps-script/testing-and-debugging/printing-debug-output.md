---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:58:27.039199-07:00
description: "\xC5 skrive ut feils\xF8kingsutdata inneb\xE6rer strategisk plassering\
  \ av loggutsagn i koden din for \xE5 vise variabelverdier, utf\xF8relsesflyt eller\
  \ meldingsfeil\u2026"
lastmod: '2024-03-13T22:44:40.316914-06:00'
model: gpt-4-0125-preview
summary: "\xC5 skrive ut feils\xF8kingsutdata inneb\xE6rer strategisk plassering av\
  \ loggutsagn i koden din for \xE5 vise variabelverdier, utf\xF8relsesflyt eller\
  \ meldingsfeil under kj\xF8retid."
title: "Utskrift av feils\xF8kingsdata"
weight: 33
---

## Hvordan:
Google Apps Script tilbyr `Logger`-klassen for grunnleggende feilsøking, og for mer avanserte behov, `console`-klassen introdusert i V8 kjøretidsmiljøet.

**Bruke Logger:**

Logger-klassen lar deg logge feilsøkingsmeldinger, som du kan se etter utførelse i Apps Script Editor under `Vis > Logger`. Her er et enkelt eksempel:

```javascript
function logSample() {
  var name = "Wired Reader";
  Logger.log("Hello, %s!", name);
}
```

Etter å ha kjørt `logSample()`, kan du se loggen med "Hello, Wired Reader!" i Loggviseren.

**Bruke console.log med V8 kjøretidsmiljøet:**

Med V8 kjøretidsmiljøet gir `console.log` en mer kjent syntaks for utviklere som kommer fra andre språk:

```javascript
function consoleSample() {
  var status = 'aktiv';
  var count = 150;
  console.log(`Nåværende status: ${status}, Antall: ${count}`);
}
```

Etter utførelse, tilgang Stackdriver Logging i `Vis > Stackdriver Logging` for å se utdata. Det er kraftigere, støtter tekstinterpolering og objektinspeksjon, og integreres med Google Clouds logging, som tilbyr vedvarende logger og avansert filtrering.

**Eksempel på utdata fra console.log:**

```
Nåværende status: aktiv, Antall: 150
```

## Dypdykk
Opprinnelig var `Logger.log` det primære verktøyet for feilsøking i Google Apps Script, som tilbød en enkel, grei måte å skrive ut utdata for inspeksjon. Imidlertid, etter hvert som skript blir mer komplekse og integrerte med Google Cloud Platform-tjenester, ble behovet for en mer robust loggløsning tydelig.

Enter V8 kjøretidsmiljøet, som bringer `console.log` inn i bildet. Dette ikke bare justerer Google Apps Script med standard JavaScript-syntaks, noe som gjør språket mer tilgjengelig for utviklere kjent med JavaScript men også utnytter den kraftige infrastrukturen til Google Clouds loggfunksjoner. Introduksjonen av `console.log` og dets integrasjon med Google Cloud Platform markerer en betydelig utvikling i feilsøkingsmuligheter innen Google Apps Script, og gir utviklere en mer dynamisk og skalerbar tilnærming til overvåking og feilsøking av skriptene deres.

Mens `Logger.log` er tilstrekkelig for grunnleggende feilsøkingsbehov og små prosjekter, tilbyr `console.log` med V8 kjøretidsmiljøet en mer omfattende og fremtidssikker løsning. Dette inkluderer evnen til å beholde logger utover utførelsessesjonen, søke og filtrere logger i Google Cloud-konsollen, og den generelle justeringen med moderne JavaScript-utviklingspraksis. Men, utviklere bør veie sine behov mot kompleksiteten og skalaen av prosjektene sine når de velger mellom disse alternativene.
