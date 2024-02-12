---
title:                "Söka och ersätta text"
aliases:
- /sv/google-apps-script/searching-and-replacing-text/
date:                  2024-02-01T22:01:28.251797-07:00
model:                 gpt-4-0125-preview
simple_title:         "Söka och ersätta text"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/google-apps-script/searching-and-replacing-text.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att söka och ersätta text i Google Apps Script innebär att man programmeringsmässigt identifierar specifika strängar i ett dokument, kalkylblad eller någon annan typ av Google Apps-innehåll och ersätter dem med andra textvärden. Programmerare använder denna funktionalitet för att automatisera redigeringen av stora mängder innehåll, korrigera vanliga fel, standardisera terminologi över dokument eller infoga dynamisk data i mallar.

## Hur man gör:

Google Apps Script erbjuder ett enkelt sätt att söka och ersätta text, särskilt inom Google Dokument och Kalkylark. Nedan finns exempel för båda.

### Google Dokument:

För att söka och ersätta text i ett Google Dokument, kommer du huvudsakligen att interagera med klassen `DocumentApp`.

```javascript
function searchReplaceInDoc() {
  var doc = DocumentApp.getActiveDocument();
  var body = doc.getBody();
  
  // För att söka och ersätta en specifik fras
  body.replaceText('searchText', 'replacementText');
  
  DocumentApp.getActiveDocument().saveAndClose();
}

// Användning
searchReplaceInDoc();
```

Denna kodsnutt söker efter alla förekomster av `'searchText'` i det aktiva Google Dokumentet och ersätter dem med `'replacementText'`.

### Google Kalkylark:

Liknande, i Google Kalkylark, kan du använda `SpreadsheetApp` för att genomföra sök- och ersättningsoperationer:

```javascript
function searchReplaceInSheet() {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
  
  // Söka och ersätta i det nuvarande aktiva kalkylarket
  // replaceText(searchText, replacement)
  sheet.createTextFinder('searchText').replaceAllWith('replacementText');
}

// Användning
searchReplaceInSheet();
```

I detta exempel söker `createTextFinder('searchText')` i det aktiva kalkylarket efter 'searchText', och `replaceAllWith('replacementText')` ersätter alla förekomster med 'replacementText'.

## Fördjupning

Sök- och ersättningsfunktionaliteten i Google Apps Script är starkt påverkad av dess webbaserade natur, vilket möjliggör att skript kan manipulera text över olika Google Apps sömlöst. Historiskt sett härstammar denna kapacitet från det bredare sammanhanget av textbearbetning och manipulation inom programmering, där reguljära uttryck och strängfunktioner i språk som Perl och Python satt en hög standard för flexibilitet och kraft.

Även om Google Apps Script sök- och ersättningsfunktionalitet är kraftfull för enkla substitutioner, saknar den fullständiga kapacitet för reguljära uttryck som finns i vissa andra språk. Till exempel, medan du kan använda grundläggande reguljära uttryck i `createTextFinder` i Google Kalkylark, är alternativen för komplex mönstermatchning och manipulation begränsade jämfört med Perl eller Python.

För mer avancerade textbearbetningsbehov kan programmerare behöva exportera Google Dokument- eller Kalkylarksinnehållet till ett format som kan bearbetas externt med kraftfullare språk, eller använda Google Apps Script för att anropa externa API:er eller tjänster som erbjuder mer sofistikerade textmanipulationsmöjligheter.

Trots dessa begränsningar, för de flesta typiska sök- och ersättningsuppgifter inom ekosystemet för Google Apps, erbjuder Google Apps Script en enkel, effektiv och högt integrerbar lösning skräddarsydd för behoven av automatisering och skriptning inom Googles produktivitetsverktyg.
