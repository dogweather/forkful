---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:15.088845-07:00
description: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende godkjenning involverer\
  \ koding av et brukernavn og passord i en foresp\xF8rselsoverskrift for \xE5 f\xE5\
  \ tilgang til\u2026"
lastmod: '2024-03-13T22:44:40.313659-06:00'
model: gpt-4-0125-preview
summary: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende godkjenning involverer\
  \ koding av et brukernavn og passord i en foresp\xF8rselsoverskrift for \xE5 f\xE5\
  \ tilgang til beskyttede ressurser."
title: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering"
weight: 45
---

## Hva & Hvorfor?

Å sende en HTTP-forespørsel med grunnleggende godkjenning involverer koding av et brukernavn og passord i en forespørselsoverskrift for å få tilgang til beskyttede ressurser. Programmerere bruker denne metoden for godkjenning på serversiden, for å integrere med APIer som krever grunnleggende auth for operasjoner som datahenting eller innlegging av innhold.

## Hvordan:

I Google Apps Script, for å sende en HTTP-forespørsel med grunnleggende godkjenning, bruker du `UrlFetchApp`-tjenesten kombinert med en base64-kodet autorisasjonsoverskrift. Her er en trinn-for-trinn-guide:

1. **Kode Innloggingsinformasjon**: Først, kod brukernavnet og passordet ditt i base64. Google Apps Script har ikke en innebygd base64-kodingsfunksjon for strenger, så du vil bruke Utilities.base64Encode for dette formålet.

```javascript
var brukernavn = 'DittBrukernavn';
var passord = 'DittPassord';
var kodetInnloggingsinfo = Utilities.base64Encode(brukernavn + ':' + passord);
```

2. **Sett Opp Forespørselsalternativer**: Med de kodete legitimasjonene klare, forbered alternativsobjektet for HTTP-forespørselen, inkludert metoden og overskriftene.

```javascript
var alternativer = {
  metode: 'get', // eller 'post', 'put', avhengig av dine behov
  overskrifter: {
    'Authorization': 'Basic ' + kodetInnloggingsinfo
  }
  // tilleggsalternativer som 'muteHttpExceptions' for feilhåndtering kan legges til her
};
```

3. **Gjør Forespørselen**: Bruk `UrlFetchApp.fetch`-metoden med mål-URLen og alternativsobjektet.

```javascript
var url = 'https://example.com/api/resource';
var respons = UrlFetchApp.fetch(url, alternativer);
Logger.log(respons.getContentText());
```

Eksempelutdata ved vellykket forespørsel vil variere basert på APIets respons. For et JSON-basert API, kan du se noe som:

```
{"status":"Success","data":"Resource data here..."}
```

Sørg for at du håndterer mulige HTTP-feil ved å sjekke responskoden eller bruke `muteHttpExceptions`-alternativet for mer kontrollert feiladministrasjon.

## Dypdykk

Å sende en HTTP-forespørsel med grunnleggende godkjenning har vært en standard metode i mange programmeringsspråk for å få tilgang til web-baserte ressurser som krever autentisering. I konteksten av Google Apps Script, gir `UrlFetchApp` en enkel måte å utføre disse HTTP-forespørslene på, inkludert de som krever autentisering. Inkluderingen av grunnleggende legitimasjon i forespørselsoverskriftene er en enkel, men effektiv metode, men kommer med sikkerhetsadvarsler, hovedsakelig fordi legitimasjonen sendes i klartekst, bare base64-kodet, som enkelt kan dekodes hvis den blir avlyttet.

For bedre sikkerhet anbefales alternativer som OAuth 2.0, spesielt når man håndterer sensitiv data eller operasjoner. Google Apps Script har innebygd støtte for OAuth 2.0 med `OAuth2`-biblioteket, som effektiviserer prosessen med å godkjenne mot tjenester som støtter dette protokollen.

Til tross for sine sikkerhetsbegrensninger, forblir grunnleggende godkjenning mye brukt for enkle eller interne applikasjoner som ikke er utsatt for det bredere internettet. Det er enkelt å implementere, ettersom det bare krever en enkelt forespørsel med riktig satt overskrifter, noe som gjør det til et tiltalende alternativ for raske integrasjoner eller for APIer der høyere sikkerhetsmetoder ikke er tilgjengelige. Programmerere oppfordres imidlertid til å vurdere sikkerhetsimplikasjonene og utforske tryggere alternativer når de er tilgjengelige.
