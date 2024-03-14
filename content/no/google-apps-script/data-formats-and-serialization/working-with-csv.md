---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:02.763024-07:00
description: "\xC5 jobbe med CSV-filer (Comma-Separated Values) i Google Apps Script\
  \ inneb\xE6rer \xE5 lese, endre og skrive rene tekstfiler der hver linje representerer\
  \ en\u2026"
lastmod: '2024-03-13T22:44:40.340397-06:00'
model: gpt-4-0125-preview
summary: "\xC5 jobbe med CSV-filer (Comma-Separated Values) i Google Apps Script inneb\xE6\
  rer \xE5 lese, endre og skrive rene tekstfiler der hver linje representerer en\u2026"
title: Arbeide med CSV
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å jobbe med CSV-filer (Comma-Separated Values) i Google Apps Script innebærer å lese, endre og skrive rene tekstfiler der hver linje representerer en datapost med verdier separert av kommaer. Programmerere gjør dette for å enkelt utveksle data mellom ulike applikasjoner, databaser eller programmeringsspråk på grunn av CSVs brede adopsjon som et enkelt, tekstbasert datautvekslingsformat.

## Hvordan:

### Lese CSV Data

For å lese CSV-data fra en fil lagret i Google Drive, må du først hente filens innhold som en streng, deretter analysere det. Google Apps Script gjør det enkelt å hente filinnhold med DriveApp-tjenesten.

```javascript
function readCSV() {
  var fileId = 'DITT_FILE_ID_HER'; // Erstatt med faktisk fil-ID
  var file = DriveApp.getFileById(fileId);
  var content = file.getBlob().getDataAsString();
  var rows = content.split("\n");
  
  for (var i = 0; i < rows.length; i++) {
    var cells = rows[i].split(",");
    Logger.log(cells); // Logger hver rad sine celler
  }
}
```

### Skrive CSV Data

Å opprette og skrive til en CSV innebærer å konstruere en streng med komma-separerte verdier og linjeskift, deretter lagre eller eksportere den. Dette eksemplet demonstrerer hvordan man oppretter en ny CSV-fil i Google Drive.

```javascript
function writeCSV() {
  var folderId = 'DITT_FOLDER_ID_HER'; // Erstatt med ID-en til Drive-mappen der den nye filen skal opprettes
  var csvContent = "Name,Age,Occupation\nJohn Doe,29,Ingeniør\nJane Smith,34,Designer";
  var fileName = "eksempel.csv";
  
  var folder = DriveApp.getFolderById(folderId);
  folder.createFile(fileName, csvContent, MimeType.PLAIN_TEXT);
}
```

### Eksempel på resultat

Når man logger radceller fra lesing av en CSV:

```plaintext
[John, 29, Ingeniør]
[Jane, 34, Designer]
```

Når man skriver, opprettes en fil med navnet "eksempel.csv" med innholdet:

```plaintext
Name,Age,Occupation
John Doe,29,Ingeniør
Jane Smith,34,Designer
```

## Dypdykk

Historisk sett har CSV-filer blitt foretrukket for deres enkelhet og menneskelesbarhet, noe som gjør dem tilgjengelige for ikke-programmerere og nyttige for raske datainspeksjonsoppgaver. Imidlertid opererer Google Apps Script innenfor riket av Googles økosystem, hvor Google Sheets fungerer som et kraftig, brukervennlig alternativ for manipulering av CSV. Sheets tilbyr ikke bare et GUI for redigering av data, men støtter også komplekse formler, stilsetting og mange flere funksjoner som rå CSV mangler.

Til tross for fordelene som tilbys av Google Sheets, forblir direkte manipulering av CSV i Google Apps Script viktig for automatiserte oppgaver, spesielt når man har å gjøre med eksterne systemer som genererer eller krever data i CSV-format. For eksempel integrering med arvesystemer, eksport av data for bruk i andre applikasjoner, eller forbehandling før dataene mates inn i Google Sheets.

Videre kan Google Apps Scripts evne til å jobbe med CSV-filer utvides med Utilities-tjenesten for avanserte kodingbehov, eller grensesnittes med eksterne API-er for konvertering, parsing eller valideringsoppgaver. Imidlertid, for arbeid med store datasett eller krever komplekse manipulasjoner, vurder å utnytte Google Sheets API-er eller utforske BigQuery for mer robust datahåndteringskapasiteter.

Selv om enkelheten forblir en nøkkelgrunn for CSVs popularitet, tilbyr disse alternativene et rikere sett med funksjoner for håndtering av data i det ekspansive Google Cloud-økosystemet.
