---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:58.778489-07:00
description: 'Hoe te: #.'
lastmod: '2024-03-13T22:44:50.356053-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Werken met CSV
weight: 37
---

## Hoe te:


### CSV-gegevens lezen
Om CSV-gegevens uit een bestand op Google Drive te lezen, moet u eerst de inhoud van het bestand als een string verkrijgen, vervolgens parseert u het. Google Apps Script maakt het ophalen van bestandsinhoud eenvoudig met de DriveApp-service.

```javascript
function readCSV() {
  var fileId = 'YOUR_FILE_ID_HERE'; // Vervang door de daadwerkelijke bestands-ID
  var file = DriveApp.getFileById(fileId);
  var content = file.getBlob().getDataAsString();
  var rows = content.split("\n");
  
  for (var i = 0; i < rows.length; i++) {
    var cells = rows[i].split(",");
    Logger.log(cells); // Log de cellen van elke rij
  }
}
```

### CSV-gegevens schrijven
Het maken en schrijven naar een CSV houdt in dat u een string construeert met komma-gescheiden waarden en nieuwe regels, en deze vervolgens opslaat of exporteert. Dit voorbeeld toont het maken van een nieuw CSV-bestand in Google Drive.

```javascript
function writeCSV() {
  var folderId = 'YOUR_FOLDER_ID_HERE'; // Vervang door de ID van de Drive-map waar het nieuwe bestand zal worden gemaakt
  var csvContent = "Name,Age,Occupation\nJohn Doe,29,Engineer\nJane Smith,34,Designer";
  var fileName = "example.csv";
  
  var folder = DriveApp.getFolderById(folderId);
  folder.createFile(fileName, csvContent, MimeType.PLAIN_TEXT);
}
```

### Voorbeelduitvoer
Bij het loggen van rijcellen bij het lezen van een CSV:

```plaintext
[John, 29, Engineer]
[Jane, 34, Designer]
```

Bij het schrijven wordt een bestand met de naam "example.csv" gecreëerd met de inhoud:

```plaintext
Name,Age,Occupation
John Doe,29,Engineer
Jane Smith,34,Designer
```

## Diepere duik
Historisch gezien zijn CSV-bestanden favoriet vanwege hun eenvoud en leesbaarheid voor mensen, waardoor ze toegankelijk zijn voor niet-programmeurs en nuttig voor snelle gegevensinspectietaken. Echter, Google Apps Script opereert binnen het ecosysteem van Google, waar Google Sheets fungeert als een krachtig, gebruiksvriendelijk alternatief voor CSV-manipulatie. Sheets bieden niet alleen een GUI voor het bewerken van gegevens, maar ondersteunen ook complexe formules, styling en vele andere functies die ruwe CSV's missen.

Ondanks de voordelen die Google Sheets biedt, blijft directe CSV-manipulatie in Google Apps Script belangrijk voor geautomatiseerde taken, vooral wanneer wordt omgegaan met externe systemen die gegevens genereren of vereisen in CSV-formaat. Bijvoorbeeld, integratie met legacy-systemen, het exporteren van gegevens voor gebruik in andere applicaties, of preprocessing voordat gegevens in Google Sheets worden gevoerd.

Bovendien kan de mogelijkheid van Google Apps Script om met CSV-bestanden te werken worden uitgebreid met de Utilities-service voor geavanceerde coderingsbehoeften, of worden gekoppeld aan externe API's voor conversie, parsing of validatietaken. Echter, voor het werken met grote datasets of het vereisen van complexe manipulaties, overweeg het gebruik van Google Sheets API's of verken BigQuery voor robuustere gegevensverwerkingsmogelijkheden.

Terwijl eenvoud een belangrijke reden blijft voor de populariteit van CSV, bieden deze alternatieven een rijker set functies voor het omgaan met gegevens in het uitgebreide Google Cloud-ecosysteem.
