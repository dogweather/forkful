---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:07.605776-07:00
description: "Zoeken en vervangen van tekst in Google Apps Script houdt in het programmatisch\
  \ identificeren van specifieke strings in een document, spreadsheet of elk\u2026"
lastmod: 2024-02-19 22:05:09.401428
model: gpt-4-0125-preview
summary: "Zoeken en vervangen van tekst in Google Apps Script houdt in het programmatisch\
  \ identificeren van specifieke strings in een document, spreadsheet of elk\u2026"
title: Zoeken en vervangen van tekst
---

{{< edit_this_page >}}

## Wat & Waarom?

Zoeken en vervangen van tekst in Google Apps Script houdt in het programmatisch identificeren van specifieke strings in een document, spreadsheet of elk ander type Google Apps-content, en deze te vervangen door andere tekstwaarden. Programmeurs gebruiken deze functionaliteit om het bewerken van grote hoeveelheden content te automatiseren, veelvoorkomende fouten te corrigeren, terminologie over documenten heen te standaardiseren of dynamische gegevens in sjablonen in te voegen.

## Hoe te:

Google Apps Script biedt een eenvoudige manier om tekst te zoeken en te vervangen, vooral binnen Google Documenten en Spreadsheets. Hieronder staan voorbeelden voor beide.

### Google Documenten:

Om tekst te zoeken en te vervangen in een Google Document, zul je voornamelijk interacteren met de `DocumentApp` klasse.

```javascript
function searchReplaceInDoc() {
  var doc = DocumentApp.getActiveDocument();
  var body = doc.getBody();
  
  // Om een specifieke frase te zoeken en te vervangen
  body.replaceText('searchText', 'replacementText');
  
  DocumentApp.getActiveDocument().saveAndClose();
}

// Gebruik
searchReplaceInDoc();
```

Deze code zoekt naar alle voorkomens van `'searchText'` in het actieve Google Document en vervangt ze door `'replacementText'`.

### Google Spreadsheets:

Op een vergelijkbare manier kun je in Google Spreadsheets `SpreadsheetApp` gebruiken om zoek- en vervangbewerkingen uit te voeren:

```javascript
function searchReplaceInSheet() {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
  
  // Zoeken en vervangen in het huidige actieve blad
  // replaceText(searchText, replacement)
  sheet.createTextFinder('searchText').replaceAllWith('replacementText');
}

// Gebruik
searchReplaceInSheet();
```

In dit voorbeeld zoekt `createTextFinder('searchText')` het actieve blad voor 'searchText', en `replaceAllWith('replacementText')` vervangt alle voorkomens met 'replacementText'.

## Diepgaand

De zoek- en vervangfunctionaliteit in Google Apps Script is sterk beïnvloed door zijn webgebaseerde aard, waardoor scripts tekst over verschillende Google Apps naadloos kunnen manipuleren. Historisch gezien komt deze mogelijkheid voort uit de bredere context van tekstverwerking en -manipulatie in programmering, waar reguliere expressies en stringfuncties in talen zoals Perl en Python een hoge standaard zetten voor flexibiliteit en kracht.

Hoewel de zoek- en vervangfunctionaliteit van Google Apps Script krachtig is voor eenvoudige substituties, mist het de volledige reguliere expressiemogelijkheden die in sommige andere talen worden gevonden. Je kunt bijvoorbeeld basis reguliere expressies gebruiken in `createTextFinder` in Google Spreadsheets, maar de opties voor complexe patroonmatching en -manipulatie zijn beperkt in vergelijking met Perl of Python.

Voor meer geavanceerde tekstverwerkingsbehoeften, kunnen programmeurs ervoor kiezen om de Google Documenten of Spreadsheets content te exporteren naar een formaat dat extern kan worden verwerkt met krachtigere talen of Google Apps Script gebruiken om externe API's of diensten aan te roepen die geavanceerdere tekstmanipulatiecapaciteiten bieden.

Ondanks deze beperkingen, voor de meeste typische zoek- en vervangtaken binnen het ecosysteem van Google Apps, biedt Google Apps Script een eenvoudige, efficiënte en sterk integreerbare oplossing aangepast aan de behoeften van automatisering en scripting binnen Google's suite van productiviteitstools.
