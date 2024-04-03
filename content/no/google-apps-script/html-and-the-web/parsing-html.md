---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:44.287959-07:00
description: "Hvordan: Google Apps Script har ikke en innebygd metode for parsing\
  \ av HTML. Derimot kan du utnytte `UrlFetchApp`-tjenesten for \xE5 hente HTML-innhold\
  \ og\u2026"
lastmod: '2024-03-13T22:44:40.311516-06:00'
model: gpt-4-0125-preview
summary: Google Apps Script har ikke en innebygd metode for parsing av HTML.
title: Analysering av HTML
weight: 43
---

## Hvordan:
Google Apps Script har ikke en innebygd metode for parsing av HTML. Derimot kan du utnytte `UrlFetchApp`-tjenesten for å hente HTML-innhold og deretter bruke JavaScript-metoder eller regex (regulære uttrykk) for parsing. Nedenfor er et grunnleggende eksempel på hvordan du kan hente og parse title-taggen fra en nettside.

```javascript
function parseHTMLTitle(url) {
  // Henter HTML-innholdet til nettsiden
  const response = UrlFetchApp.fetch(url);
  const htmlContent = response.getContentText();

  // Bruker et enkelt regex for å finne innholdet av <title>-taggen
  const titleRegex = /<title>(.*?)<\/title>/;
  const match = htmlContent.match(titleRegex);

  // Sjekker om en title ble funnet og returnerer den
  if (match && match.length > 1) {
    return match[1];
  }

  return 'Ingen tittel funnet';
}

// Eksempel på bruk
const url = 'http://example.com';
const pageTitle = parseHTMLTitle(url);
Logger.log(pageTitle); // Skriver ut tittelen på nettsiden
```

For en mer sofistikert HTML-parsing, kan du bruke `XmlService` for å parse HTML som XML. Merk at dette krever at HTML-en er vel-formet XML, noe som ikke alltid er tilfellet:

```javascript
function parseHTMLUsingXmlService(htmlContent) {
  try {
    const document = XmlService.parse(htmlContent);
    const rootElement = document.getRootElement();
    // Herfra kan du navigere i XML-treet med XmlService-metoder
    // For eksempel for å finne et spesifikt element eller attributt
  } catch(e) {
    Logger.log('Feil ved parsing av HTML: ' + e.toString());
  }
}
```

## Dypdykk:
Historisk sett har HTML-parsing i miljøer som Google Apps Script vært utfordrende på grunn av mangel på et Dokumentobjektmodell (DOM) eller dedikerte parsingbiblioteker som er vanlige i andre programmeringskontekster. JavaScript i en nettleser har for eksempel DOM lett tilgjengelig, og Node.js-miljøer har tilgang til en mengde NPM-pakker som `cheerio` eller `jsdom` for parsing av HTML.

Google Apps Scripts tilnærming støtter seg sterkt på å bruke `UrlFetchApp` for nettforspørsler og deretter manipulere responsdata ved hjelp av enten regex eller XML-parsingmetoder. Selv om regex kan være nyttig for enkle parsingoppgaver, er det generelt sett ikke anbefalt for kompleks HTML på grunn av risikoen for feil og den potensielt skjøre kodenaturen. XML-parsing med `XmlService` tilbyr en mer strukturert tilnærming, men krever vel-formet HTML/XML, noe som kan være en begrensning når man har med vilkårlige nettsider å gjøre.

For komplekse parsingbehov, eller når man håndterer dårlig formatert HTML, kan en alternativ strategi inkludere bruk av en webtjeneste ekstern til Google Apps Script. Denne tjenesten kan behandle HTML-innhold, muligens ved hjelp av en mer robust parsingsteknikk eller bibliotek, og deretter returnere de bearbeidede dataene i en form som lett kan brukes av Google Apps Script. Denne tilnærmingen introduserer imidlertid nettverksforsinkelser og kompleksiteten av å håndtere en ekstra webtjeneste.

Til tross for disse utfordringene, forblir parsing av HTML innen Google Apps Script et kraftfullt verktøy, spesielt når det kombineres med andre Google-tjenester og APIer, og tilbyr et spekter av automatiseringsmuligheter som kan øke produktiviteten og datahåndteringsevner betraktelig.
