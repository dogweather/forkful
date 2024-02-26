---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:48.558996-07:00
description: "\xC5 skrive til standardfeil (stderr) i programmeringsspr\xE5k handler\
  \ om \xE5 dirigere feilmeldinger og diagnostikk til en separat str\xF8m, bort fra\
  \ standard utgang\u2026"
lastmod: '2024-02-25T18:49:38.562893-07:00'
model: gpt-4-0125-preview
summary: "\xC5 skrive til standardfeil (stderr) i programmeringsspr\xE5k handler om\
  \ \xE5 dirigere feilmeldinger og diagnostikk til en separat str\xF8m, bort fra standard\
  \ utgang\u2026"
title: Skrive til standardfeil
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å skrive til standardfeil (stderr) i programmeringsspråk handler om å dirigere feilmeldinger og diagnostikk til en separat strøm, bort fra standard utgang (stdout). Programmerere gjør dette for å skille vanlig programutdata fra feilmeldinger, noe som gjør feilsøking og logganalyse mer rett frem.

## Hvordan:

Google Apps Script, som er et skriptspråk for lettviktsapplikasjonsutvikling på Google Apps-plattformen, tilbyr ikke en direkte innebygd funksjon som `console.error()` for å skrive til stderr, som du kanskje finner i Node.js eller Python. Du kan imidlertid simulere denne oppførselen ved å bruke Google Apps Scripts loggtjenester eller egendefinert feilhåndtering for å håndtere og segregere feilutdata.

### Eksempel: Bruke `Logger` for feilmeldinger

```javascript
function logError() {
  try {
    // Simuler en feil
    const result = 1 / 0;
    if(!isFinite(result)) throw new Error("Forsøkt divisjon med null");
  } catch (e) {
    // Skriv feilmelding til Logger
    Logger.log('Feil: ' + e.message);
  }
}
```

Når du kjører `logError()`, vil dette skrive feilmeldingen til Google Apps Scripts logg, som du kan se ved å velge `Vis > Logger`. Dette er ikke akkurat stderr, men det tjener et lignende formål om å skille feillogger fra standard utdata.

### Avansert diagnostisk logging

For mer avansert feilsøking og feillogging, kan du bruke Stackdriver Logging, nå kjent som Google Clouds Operations Suite.

```javascript
function advancedErrorLogging() {
  try {
    // Forårsak en feil med vilje
    const obj = null;
    const result = obj.someProperty;
  } catch (e) {
    console.error('Feil oppdaget: ', e.toString());
  }
}
```

Dette vil dirigere feilmeldingen til Stackdriver Logging, hvor den behandles som en feilnivålogg. Merk at Stackdriver/Google Clouds Operations Suite-integrasjon tilbyr en mer detaljert og søkbar loggløsning sammenlignet med `Logger`.

## Dypdykk

Mangelen på en dedikert `stderr`-strøm i Google Apps Script reflekterer dets natur og opphav som et skybasert skriptspråk, hvor tradisjonelle konsoll- eller terminalbaserte utdata (som stdout og stderr) er mindre relevante. Historisk sett ble Google Apps Script designet for å forbedre Google Apps-funksjonalitet med enkle skript, med fokus på brukervennlighet fremfor omfattende funksjoner som er tilgjengelige i mer komplekse programmeringsmiljøer.

Det sagt, utviklingen av Google Apps Script mot mer sofistikert applikasjonsutvikling har oppfordret utviklere til å adoptere kreative tilnærminger til feilhåndtering og logging, ved å benytte tilgjengelige tjenester som Logger og integrere med Google Clouds Operations Suite. Disse metodene, selv om de ikke er direkte stderr-implementasjoner, tilbyr robuste alternativer for feilhåndtering og diagnostisk logging i et sky-sentrert miljø.

Kritisk, mens disse metodene tjener formålet innenfor Google Apps Scripts økosystem, understreker de plattformens begrensninger sammenlignet med tradisjonelle programmeringsmiljøer. For utviklere som krever detaljert og hierarkisk feilhåndteringsstrategier, kan integrering med eksterne loggtjenester eller å adoptere Google Cloud Functions, som tilbyr en mer konvensjonell håndtering av stderr og stdout, være å foretrekke.
