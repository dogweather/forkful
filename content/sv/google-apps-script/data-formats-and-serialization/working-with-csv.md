---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:23.713105-07:00
description: "Hur man g\xF6r: F\xF6r att l\xE4sa CSV-data fr\xE5n en fil som \xE4\
  r lagrad i Google Drive, beh\xF6ver du f\xF6rst h\xE4mta filens inneh\xE5ll som\
  \ en str\xE4ng f\xF6r att sedan tolka det.\u2026"
lastmod: '2024-03-13T22:44:37.463272-06:00'
model: gpt-4-0125-preview
summary: "F\xF6r att l\xE4sa CSV-data fr\xE5n en fil som \xE4r lagrad i Google Drive,\
  \ beh\xF6ver du f\xF6rst h\xE4mta filens inneh\xE5ll som en str\xE4ng f\xF6r att\
  \ sedan tolka det."
title: Att Arbeta med CSV
weight: 37
---

## Hur man gör:


### Läsa CSV-data
För att läsa CSV-data från en fil som är lagrad i Google Drive, behöver du först hämta filens innehåll som en sträng för att sedan tolka det. Google Apps Script gör det enkelt att hämta filinnehåll med DriveApp-tjänsten.

```javascript
function readCSV() {
  var fileId = 'DITT_FIL_ID_HÄR'; // Ersätt med faktiskt fil-ID
  var file = DriveApp.getFileById(fileId);
  var content = file.getBlob().getDataAsString();
  var rows = content.split("\n");
  
  for (var i = 0; i < rows.length; i++) {
    var cells = rows[i].split(",");
    Logger.log(cells); // Loggar varje rads celler
  }
}
```

### Skriva CSV-data
Att skapa och skriva till en CSV innebär att konstruera en sträng med komma-separerade värden och radbrytningar, för att sedan spara eller exportera den. Detta exempel demonstrerar skapandet av en ny CSV-fil i Google Drive.

```javascript
function writeCSV() {
  var folderId = 'DITT_MAPP_ID_HÄR'; // Ersätt med ID for mappen där den nya filen ska skapas
  var csvContent = "Namn,Ålder,Yrke\nJohn Doe,29,Ingenjör\nJane Smith,34,Designer";
  var fileName = "exempel.csv";
  
  var folder = DriveApp.getFolderById(folderId);
  folder.createFile(fileName, csvContent, MimeType.PLAIN_TEXT);
}
```

### Exempel på utdata
När man loggar cellraderna från att ha läst en CSV:

```plainttext
[John, 29, Ingenjör]
[Jane, 34, Designer]
```

När man skriver, skapas en fil med namnet "exempel.csv" med innehållet:

```plaintext
Namn,Ålder,Yrke
John Doe,29,Ingenjör
Jane Smith,34,Designer
```

## Fördjupning
Historiskt sett har CSV-filer varit omtyckta för deras enkelhet och läsbarhet för människor, vilket gjort dem tillgängliga för icke-programmerare och användbara för snabba uppgifter för datainspektion. Dock, Google Apps Script verkar inom ramen för Googles ekosystem, där Google Kalkylark fungerar som ett kraftfullt, användarvänligt alternativ för manipulering av CSV. Kalkylark erbjuder inte bara ett grafiskt användargränssnitt för att redigera data men stöder också komplexa formler, styling och många fler funktioner som råa CSV-filer saknar.

Trots fördelarna som erbjuds av Google Kalkylark, är direkt manipulering av CSV i Google Apps Script fortfarande viktigt för automatiserade uppgifter, särskilt när man hanterar externa system som genererar eller kräver data i CSV-format. Till exempel, integration med äldre system, exportera data för användning i andra applikationer eller förbehandling innan data matas in i Google Kalkylark.

Dessutom kan Google Apps Scripts förmåga att arbeta med CSV-filer utökas med Utilities-tjänsten för avancerade kodningsbehov, eller kopplas samman med externa API:er för konvertering, tolkning eller valideringsuppgifter. Dock, för arbete med stora datamängder eller behov av komplexa manipulationer, överväg att använda Google Kalkylarks API:er eller utforska BigQuery för mer robusta databehandlingskapaciteter.

Medan enkelhet fortfarande är en viktig anledning till CSV:s popularitet, erbjuder dessa alternativ en rikare uppsättning funktioner för att hantera data i det expansiva Google Cloud-ekosystemet.
