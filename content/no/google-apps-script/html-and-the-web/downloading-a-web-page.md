---
title:                "Nedlasting av en nettside"
aliases:
- /no/google-apps-script/downloading-a-web-page.md
date:                  2024-02-01T21:52:36.303703-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nedlasting av en nettside"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/google-apps-script/downloading-a-web-page.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å laste ned en nettside i Google Apps Script innebærer å hente innholdet på en nettside via HTML for ulike formål, som nettskraping, datautvinning eller overvåkning av endringer. Programmerere velger denne operasjonen for å automatisere innsamling av data eller integrasjonsoppgaver, noe som minimerer manuell innsats og sikrer behandling av data i sanntid.

## Hvordan:

I Google Apps Script er tjenesten `UrlFetchApp` avgjørende for nedlasting av webinnhold. Nedenfor er en trinn-for-trinn-guide og et enkelt eksempel som demonstrerer hvordan man henter og logger HTML-innholdet til en nettside:

1. **Grunnleggende Henteoperasjon:**

```javascript
function downloadWebPage() {
  var url = "http://example.com";
  var response = UrlFetchApp.fetch(url);
  var content = response.getContentText();
  Logger.log(content);
}
```

- Denne koden henter HTML-innholdet til example.com og logger det. Det er en grei demonstrasjon av å få tak i kilden til en nettside uten noen ekstra parametere.

2. **Håndtering av Omdirigeringer og HTTPS:**

For HTTPS eller håndtering av omdirigeringer, forblir koden stort sett den samme, men vurder å implementere feilhåndtering eller spesifikke alternativer for omdirigeringer:

```javascript
function downloadSecureWebPage() {
  var options = {
    'followRedirects': true, // Følg omdirigeringer automatisk
    'muteHttpExceptions': true // Demp mulige unntak for å håndtere dem nådig
  };
  
  var url = "https://example.com";
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

3. **Rategrenser og Kvoter:**

Vær oppmerksom på Google Apps Script's kvoter; intens bruk kan kreve feilhåndtering for rategrenser.

## Dypdykk

Historisk sett begynte nedlasting og manipulering av webinnhold med enkle HTTP-forespørsler, som har utviklet seg betydelig med introduksjonen av skriptspråk. Google Apps Script tillater enkel utførelse av slike oppgaver innenfor G Suite-økosystemet, ved å dra nytte av Googles robuste infrastruktur. Tjenesten `UrlFetchApp` er et sentralt element i denne funksjonaliteten og kapsler inn komplekse HTTP/S-forespørsler i et enklere applikasjonsnivågrensesnitt.

Tross sin bekvemmelighet, kan Google Apps Script ikke alltid være det beste verktøyet for tung nettskraping eller når kompleks etterbehandling av hentede data kreves på grunn av utførelsestidsgrenser og kvoter satt av Google. I slike tilfeller, kan dedikerte rammeverk for nettskraping eller språk designet for asynkrone I/O-operasjoner, slik som Node.js med biblioteker som Puppeteer eller Cheerio, tilby mer fleksibilitet og kraft.

Videre, selv om Google Apps Script er et utmerket verktøy for integrering med Google-tjenester (som Sheets, Docs og Drive) og utførelse av lettvektige datahenteoperasjoner, er det avgjørende å huske på begrensningene i eksekveringsmiljøet. For intensive oppgaver, vurder å bruke Google Cloud Functions eller Apps Script's avanserte tjenester med eksterne dataressurser for behandling.
