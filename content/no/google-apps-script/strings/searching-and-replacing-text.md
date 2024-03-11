---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:12.690197-07:00
description: "S\xF8king og erstatning av tekst i Google Apps Script inneb\xE6rer \xE5\
  \ programmert identifisere spesifikke strenger i et dokument, regneark eller annen\
  \ type\u2026"
lastmod: '2024-03-11T00:14:13.814288-06:00'
model: gpt-4-0125-preview
summary: "S\xF8king og erstatning av tekst i Google Apps Script inneb\xE6rer \xE5\
  \ programmert identifisere spesifikke strenger i et dokument, regneark eller annen\
  \ type\u2026"
title: "S\xF8ke og erstatte tekst"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Søking og erstatning av tekst i Google Apps Script innebærer å programmert identifisere spesifikke strenger i et dokument, regneark eller annen type Google Apps-innhold, og erstatte dem med andre tekstverdier. Programmerere bruker denne funksjonaliteten for å automatisere redigeringen av store mengder innhold, korrigere vanlige feil, standardisere terminologi på tvers av dokumenter, eller sette inn dynamiske data i maler.

## Hvordan:

Google Apps Script tilbyr en grei måte å søke og erstatte tekst på, spesielt innen Google Dokumenter og Regneark. Nedenfor er eksempler for begge.

### Google Dokumenter:

For å søke og erstatte tekst i et Google Dokument, vil du hovedsakelig samhandle med `DocumentApp`-klassen.

```javascript
function searchReplaceInDoc() {
  var doc = DocumentApp.getActiveDocument();
  var body = doc.getBody();
  
  // For å søke og erstatte en spesifikk frase
  body.replaceText('searchText', 'replacementText');
  
  DocumentApp.getActiveDocument().saveAndClose();
}

// Bruk
searchReplaceInDoc();
```

Dette kodeutsnittet søker etter alle forekomster av `'searchText'` i det aktive Google Dokumentet og erstatter dem med `'replacementText'`.

### Google Regneark:

På samme måte, i Google Regneark, kan du bruke `SpreadsheetApp` for å utføre søk- og erstatt-operasjoner:

```javascript
function searchReplaceInSheet() {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
  
  // Søk og erstatt i det nåværende aktive arket
  // replaceText(searchText, replacement)
  sheet.createTextFinder('searchText').replaceAllWith('replacementText');
}

// Bruk
searchReplaceInSheet();
```

I dette eksemplet søker `createTextFinder('searchText')` gjennom det aktive arket etter 'searchText', og `replaceAllWith('replacementText')` erstatter alle forekomster med 'replacementText'.

## Fordypning

Søk- og erstatt-funksjonaliteten i Google Apps Script er sterkt påvirket av dens nettbaserte natur, noe som tillater at skript kan manipulere tekst på tvers av ulike Google Apps sømløst. Historisk sett stammer denne evnen fra den bredere konteksten av tekstbehandling og -manipulasjon i programmering, der regulære uttrykk og strengfunksjoner i språk som Perl og Python har satt en høy standard for fleksibilitet og kraft.

Selv om Google Apps Script sin søk- og erstatt-funksjonalitet er kraftfull for enkle substitusjoner, mangler den de fullstendige regulære uttrykksfunksjonene som finnes i noen andre språk. For eksempel, mens du kan bruke grunnleggende regulære uttrykk i `createTextFinder` i Google Regneark, er alternativene for kompleks mønstersammenligning og -manipulering begrenset sammenlignet med Perl eller Python.

For mer avanserte tekstbehandlingsbehov, kan programmerere ty til å eksportere Google Dokumenter eller Regneark-innhold til et format som kan behandles eksternt med mer kraftfulle språk, eller bruke Google Apps Script til å kalle eksterne APIer eller tjenester som tilbyr mer sofistikerte tekstmanipuleringsfunksjoner.

Til tross for disse begrensningene, for mest typiske søk- og erstatt-oppgaver innenfor økosystemet av Google Apps, tilbyr Google Apps Script en enkel, effektiv og høyt integrerbar løsning skreddersydd til behovene for automatisering og skripting innen Googles pakke med produktivitetsverktøy.
